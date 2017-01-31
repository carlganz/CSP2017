library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(openxlsx)


server <- function(input, output, session) {
  my_db <- src_sqlite("../Data/metadata.sqlite3") %>%
    tbl("metadata")

  output$db <- DT::renderDataTable({
    req(input$year, input$age)
    my_db %>%
      filter_(~year %in% c(input$year,
                           input$year),
              ~age %in% c(input$age,
                          input$age)) %>%
      collect()
  }, rownames = FALSE,
  selection = list(mode = FALSE))

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
        filter_(~year %in% c(input$year,
                             input$year),
                ~age %in% c(input$age,
                            input$age)) %>%
        collect()

      db <- lapply(input$age, function(i) {
        d <- db %>% filter_(~age==i)

        lapply(unique(d$year), function(y) {
          m <- d %>% filter_(~year==y)
          names(m)[-1] <- paste0(names(m)[-1],
                                 "_", y)
          m
        }) %>% Reduce(
          function(x,y) {
            left_join(x, y, by = "varname")
          }
          ,.) %>% select_(
            ~varname,
            ~starts_with("label"),
            ~starts_with("universe"),
            ~starts_with("vartype"),
            ~starts_with("identify"),
            ~starts_with("sensitive"),
            ~starts_with("PUF"),
            ~starts_with("ddsection"),
            ~starts_with("ddorder"),
            ~starts_with("notes")
          )

      })

      names(db) <- input$age

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

  output$changes <- DT::renderDataTable({
    req(input$uploader)
    ages <- c("ADULT", "TEEN", "CHILD")
    ordering <- c("varname", "age", "year",
                  "var","value")

    newDb <- lapply(ages, function(i) {
      tryCatch(read.xlsx(input$uploader$datapath,sheet=i),
               error = function(e) {NULL})
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

      if (any(type=="character")) {
        for (age in ages) {
          newDb[[age]][[var]] <- as.character(newDb[[age]][[var]])
        }
      }
    }

    newDb <- bind_rows(newDb)

    newDb <- newDb[,!sapply(newDb, function(x) {
      all(is.na(x)) | all(x=="N/A")
    })]

    newDb <- gather(newDb, "var", "value", -varname, -age)

    newDb[, c("var","year")] <-
      reshape2::colsplit(newDb$var, "[_](?=[^_]*$)",
                         c("var","year"))
    print(newDb)
    newDb <- newDb  %>%
      select_(~varname, ~age, ~year, ~var, ~value) %>%
      arrange_(~varname, ~age, ~year)

    # new <- new[!is.na(new$value), ]

    years <- rev(unique(newDb$year))

    diffs <- my_db %>%
      filter_(~year %in% c(years,years),
              ~age %in% c(ages, ages)) %>%
      collect() %>% gather("var","value",-varname,-age,-year) %>%
      select_(~varname, ~age, ~year, ~var, ~value) %>%
      arrange_(~varname, ~age, ~year) %>%
      as.data.frame %>%
      left_join(newDb,by = c("varname","age","year", "var"),
                suffix = c(".Old",".New")) %>%
      filter_(~value.Old!=value.New)

    diffs

  })

}