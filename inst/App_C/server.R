library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(openxlsx)
library(DBI)


server <- function(input, output, session) {
  session$onSessionEnded(stopApp)

  # load sqlite db with dplyr
  my_db <- src_sqlite("www/metadata.sqlite3") %>%
    tbl("metadata")

  output$db <- DT::renderDataTable({
    req(input$year, input$age)
    my_db %>%
      filter_( ~ year %in% c(input$year,
                             input$year),
               ~ age %in% c(input$age,
                            input$age)) %>%
      collect()
  }, rownames = FALSE,
  selection = list(mode = FALSE))

  # download excel file
  output$download <- downloadHandler(
    filename = function() {
      paste("metadata", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      hs <- createStyle(
        fontColour = "#ffffff",
        fgFill = "#3284BF",
        halign = "center",
        valign = "center",
        textDecoration = "Bold",
        border = "TopBottomLeftRight"
      )
      db <- my_db %>%
        filter_( ~ year %in% c(input$year,
                               input$year),
                 ~ age %in% c(input$age,
                              input$age)) %>%
        collect()

      db <- lapply(input$age, function(i) {
        d <- db %>% filter_( ~ age == i)

        lapply(unique(d$year), function(y) {
          m <- d %>% filter_( ~ year == y)
          names(m)[-1] <- paste0(names(m)[-1],
                                 "_", y)
          m
        }) %>% Reduce(function(x, y) {
          left_join(x, y, by = "varname")
        }
        , .) %>% select_(
          ~ varname,
          ~ starts_with("label"),
          ~ starts_with("universe"),
          ~ starts_with("vartype"),
          ~ starts_with("identify"),
          ~ starts_with("sensitive"),
          ~ starts_with("PUF"),
          ~ starts_with("ddsection"),
          ~ starts_with("ddorder"),
          ~ starts_with("notes")
        )

      })

      names(db) <- input$age

      on.exit({
        reset("year")
        reset("age")
      })

      if (length(db)==1) {
        db <- db[[1]]
      }
      db %>%
        write.xlsx(
          file,
          sheetName = input$age,
          firstRow = TRUE,
          firstCol = TRUE,
          headerStyle = hs
        )
    }
  )

  # read excel file and compare to db
  difference <- reactive({
    req(input$uploader)
    ages <- c("ADULT", "TEEN", "CHILD")
    ordering <- c("varname", "age", "year",
                  "var", "value")

    newDb <- lapply(ages, function(i) {
      tryCatch(
        read.xlsx(input$uploader$datapath, sheet = i),
        error = function(e) {
          NULL
        }
      )
    })

    names(newDb) <- ages

    ages <- ages[!sapply(newDb, is.null)]

    newDb <- lapply(ages, function(i) {
      newDb[[i]]$age <- i
      newDb[[i]]
    })

    names(newDb) <- ages

    for (var in names(newDb[[1]])) {
      type <- sapply(ages, function(age) {
        class(newDb[[age]][[var]])
      })

      if (any(type == "character")) {
        for (age in ages) {
          newDb[[age]][[var]] <- as.character(newDb[[age]][[var]])
        }
      }
    }

    newDb <- bind_rows(newDb)

    newDb <- newDb[, !sapply(newDb, function(x) {
      all(is.na(x)) | all(x == "N/A")
    })]

    newDb <- gather(newDb, "var", "value",-varname,-age)

    newDb[, c("var", "year")] <-
      reshape2::colsplit(newDb$var, "[_](?=[^_]*$)",
                         c("var", "year"))
    newDb <- newDb  %>%
      select_( ~ varname, ~ age, ~ year, ~ var, ~ value) %>%
      arrange_( ~ varname, ~ age, ~ year)

    years <- rev(unique(newDb$year))

    diffs <- my_db %>%
      filter_( ~ year %in% c(years, years),
               ~ age %in% c(ages, ages)) %>%
      collect() %>% gather("var", "value", -varname, -age, -year) %>%
      select_( ~ varname, ~ age, ~ year, ~ var, ~ value) %>%
      arrange_( ~ varname, ~ age, ~ year) %>%
      as.data.frame %>%
      left_join(
        newDb,
        by = c("varname", "age", "year", "var"),
        suffix = c(".Old", ".New")
      ) %>%
      filter_( ~ value.Old != value.New)

    diffs
  })

  output$changes <- DT::renderDataTable({
    difference()

  }, selection = list(mode = FALSE))

  observe({
    toggleState("accept",
                isTruthy(input$uploader) &&
                  nrow(difference())>0)
  })

  # update db
  observeEvent(input$accept, {
    con <- my_db[[1]]$con
    difference <- difference()
    for (v in unique(difference$var)) {
      # make sure we paste sql safely
      v <- match.arg(v, choices =
                  dbListFields(con, "metadata")[-c(1,11:12)])

      q <- dbSendQuery(con,
          sprintf("update metadata set %s=? where
                  varname=? and year=? and age=?", v)
                       )
      diff <- difference %>%
        filter_(~var==v)

      dbBind(q, list(
        diff$value.New,
        diff$varname,
        diff$year,
        diff$age
      ))

      dbFetch(q)
      dbClearResult(q)

      reset("uploader")

      updateTabsetPanel(session, "which",
                        "Download")

      showModal(
        modalDialog(
          h2("Database is updated"),
          h4(
            sprintf("There were %s updates",
                    nrow(difference))
          )
        )
      )

    }

  })

}