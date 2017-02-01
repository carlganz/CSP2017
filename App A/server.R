library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)


server <- function(input, output, session) {
  # load data
  chis <- readRDS("../Data/chis.rds")

  output$varType <- reactive({
    req(input$vars)
    class(chis[[input$vars]]) == "labelled"
  })
  outputOptions(output, "varType",
                suspendWhenHidden = FALSE)

  observe({
    req(input$vars)
    updateTextInput(session,
                    "newName",
                    value = paste0(input$vars,
                                   "_P"))
    updateTextInput(session,
                    "newLabel",
                    value = paste0(attr(chis[[input$vars]], "label"),
                                   " (PUF Recode)"))
  })

  freqTable <- reactive({
    req(input$vars == "CA6")
    fmt <- as.data.frame(attr(chis[[input$vars]], "labels")) %>%
      tibble::rownames_to_column("val")
    names(fmt) <- c("Format", "Value")

    tbl <- chis %>% group_by_(input$vars) %>% summarise(Total = n())

    fmt %>% left_join(tbl, by = c(Value = input$vars)) %>%
      mutate_(Total =  ~ if_else(is.na(Total),
                                 0L, Total)) %>%
      arrange_(~ Value) %>% select_( ~ Value,  ~ Format,  ~ Total)
  })

  # first tab
  output$freq1 <- DT::renderDataTable({
    req(input$vars == "CA6")
    freqTable()
  }, rownames = FALSE, selection = list(target = "row"),
  options = list(
    paging = FALSE,
    bInfo = 0,
    bSort = 0,
    bFilter = 0,
    rowCallback = DT::JS(
      'function(row, data) {
      if (0 < parseFloat(data[2]) &&
      parseFloat(data[2]) < 100) {
      $("td", row).css("color", "red").
      css("font-weight", "bold");
      }
}'
                 )
  ))

  freqTable2 <- reactive({
    req(input$vars == "CA6", length(input$freq1_rows_selected) > 1)

    freq <- freqTable()
    freq <- freq %>% add_row(
      Format = paste0(collapse = "/",
                      freq[input$freq1_rows_selected,
                           "Format"]),
      Value = min(freq[input$freq1_rows_selected,
                       "Value"]),
      Total = sum(freq[input$freq1_rows_selected,
                       "Total"])
    )

    freq[-c(input$freq1_rows_selected),] %>%
      arrange_( ~ Value) %>% select_( ~ Value,  ~ Format,  ~ Total)
  })

  output$freq2 <- DT::renderDataTable({
    freqTable2()
  }, rownames = FALSE, selection = list(target = "row"),
  options = list(
    paging = FALSE,
    bInfo = 0,
    bSort = 0,
    bFilter = 0,
    rowCallback = DT::JS(
      'function(row, data) {
      if (0 < parseFloat(data[2]) &&
      parseFloat(data[2]) < 100) {
      $("td", row).css("color", "red").
      css("font-weight", "bold");
      }
      }'
                 )
  ), callback = DT::JS(
    paste0(
      'table.on("click.dt","tr",function() {
      var data = table.row(this).data();
      if (data[0]=="',
      min(freqTable()[input$freq1_rows_selected,
                      "Value"]),
      '".toUpperCase()) {
      alert("Do not collapse same level more than once");
      table.row(this).deselect();
      }
      })'
  )
    ))

  freqTable3 <- reactive({
    req(input$vars == "CA6", length(input$freq2_rows_selected) > 1)

    freq <- freqTable2()
    freq <- freq %>% add_row(
      Format = paste0(collapse = "/",
                      freq[input$freq2_rows_selected,
                           "Format"]),
      Value = min(freq[input$freq2_rows_selected,
                       "Value"]),
      Total = sum(freq[input$freq2_rows_selected,
                       "Total"])
    )

    freq[-c(input$freq2_rows_selected),] %>%
      arrange_( ~ Value) %>% select_( ~ Value,  ~ Format,  ~ Total)
  })

  output$freq3 <- DT::renderDataTable({
    freqTable3()
  }, rownames = FALSE, selection = list(target = "none"),
  options = list(
    paging = FALSE,
    bInfo = 0,
    bSort = 0,
    bFilter = 0,
    rowCallback = DT::JS(
      'function(row, data) {
      if (0 < parseFloat(data[2]) &&
      parseFloat(data[2]) < 100) {
      $("td", row).css("color", "red").
      css("font-weight", "bold");
      }
      }'
    )
  ))

  # second tab

  output$beforeRecode <- renderPlot({
    req(input$vars == "SRAGE")
    plot <- ggplot(chis,
                   aes_string(input$vars)) + geom_histogram() +
      ggtitle(paste0(input$vars, " before recoding")) +
      geom_text(stat = 'bin',
                aes(label = ..count..),
                vjust = -.2)
    plot
    #ggplotly(plot, tooltip = c())
  })

  output$mySlider <- renderUI({
    req(input$vars == "SRAGE")
    sliderInput(
      "recoder",
      "Select new min, and max values",
      min = min(chis[[input$vars]]),
      max = max(chis[[input$vars]]),
      value = c(min(chis[[input$vars]]),
                max(chis[[input$vars]]))
    )
  })

  recoded <- reactive({
    req(input$vars == "SRAGE", input$recoder)

    chis2 <- chis

    chis2[chis2[[input$vars]] < input$recoder[1], input$vars] <-
      input$recoder[1]
    chis2[chis2[[input$vars]] > input$recoder[2], input$vars] <-
      input$recoder[2]
    chis2
  })

  output$afterRecode <- renderPlot({
    req(input$vars == "SRAGE", input$recoder)

    chis2 <- recoded()
    plot <- ggplot(chis2,
                   aes_string(input$vars)) + geom_histogram() +
      ggtitle(paste0(input$vars, " after recoding")) +
      geom_text(stat = 'bin',
                aes(label = ..count..),
                vjust = -.2)
    plot
    # ggplotly(plot, tooltip = c())
  })

  # third tab
  counter <- reactiveValues(val = 0)
  output$count <- renderUI({
    req(input$tab == "Group")
    HTML(counter$val)
  })

  observeEvent(input$addLevel, {
    counter$val <- 1 + counter$val
  })

  observeEvent(input$removeLevel, {
    if (counter$val > 0) {
      counter$val <- -1 + counter$val
    }
  })

  output$continousGraph <- renderPlot({
    req(input$vars == "SRAGE")
    plot <- ggplot(chis,
                   aes_string(input$vars)) + geom_histogram() +
      ggtitle(paste0(input$vars, " before grouping")) +
      geom_text(stat = 'bin',
                aes(label = ..count..),
                vjust = -.2)
    plot
    #ggplotly(plot, tooltip = c())
  })

  output$sliders <- renderUI({
    req(counter$val, input$vars == "SRAGE")

    tagList(lapply(seq_len(counter$val), function(i) {
      if (i == 1) {
        fluidRow(
          column(
            width = 4,
            numericInput(
              paste0("min", i),
              label = paste0("Select minimum for level ", i, " (inclusive)"),
              min = max(c(min(chis[[input$vars]]), 0)),
              max = max(c(min(chis[[input$vars]]), 0)),
              value = max(c(min(chis[[input$vars]]), 0))
            )
          ),
          column(
            width = 4,
            numericInput(
              paste0("max", i),
              label = paste0("Select maximum for level ", i, " (not inclusive)"),
              min = max(c(min(chis[[input$vars]]), 0)),
              max = max(chis[[input$vars]]),
              value = input[[paste0("max", i)]]
            )
          )
          ,
          column(
            width = 4,
            textInput(
              paste0("level", i),
              value = paste0("Less than ",
                             input[[paste0("max", i)]]),
              label = paste0("Choose title for the new level ", i)
            )
          )
        )

      } else if (i == counter$val) {
        fluidRow(
          column(
            width = 4,
            numericInput(
              paste0("min", i),
              label = paste0("Select minimum for level ", i, " (inclusive)"),
              min = input[[paste0("max", i - 1)]],
              max = max(c(min(chis[[input$vars]]), 0)),
              value = input[[paste0("max", i - 1)]]
            )
          ),
          column(
            width = 4,
            numericInput(
              paste0("max", i),
              label = paste0("Select maximum for level ", i, " (inclusive)"),
              min = max(chis[[input$vars]]),
              max = max(chis[[input$vars]]),
              value = max(chis[[input$vars]])
            )
          ),
          column(
            width = 4,
            textInput(
              paste0("level", i),
              value = paste0("Greater than and including ",
                             input[[paste0("min", i)]]),
              label = paste0("Choose title for the new level ", i)
            )
          )
        )
      } else {
        fluidRow(
          column(
            width = 4,
            numericInput(
              paste0("min", i),
              label = paste0("Select minimum for level ", i, " (inclusive)"),
              min = input[[paste0("max", i - 1)]],
              max = max(chis[[input$vars]]),
              value = input[[paste0("max", i - 1)]]
            )
          ),
          column(
            width = 4,
            numericInput(
              paste0("max", i),
              label = paste0("Select maximum for level ", i, " (not inclusive)"),
              min = input[[paste0("min", i)]],
              max = max(chis[[input$vars]]),
              value = input[[paste0("max", i)]]
            )
          ),
          column(
            width = 4,
            textInput(
              paste0("level", i),
              value = paste0("[", input[[paste0("min", i)]], ",",
                             input[[paste0("max", i)]], ")"),
              label = paste0("Choose title for the new level ", i)
            )
          )
        )
      }
    }))
  })

  discrete <- reactive({
    req(input$vars == "SRAGE", counter$val > 1)

    oldVar <- chis[[input$vars]]
    newVar <- character(nrow(chis))
    f <- character(counter$val)
    for (i in seq_len(counter$val)) {
      if (i != counter$val) {
        rows <- oldVar >= input[[paste0("min", i)]] &
          oldVar < input[[paste0("max", i)]]
      } else {
        rows <- oldVar >= input[[paste0("min", i)]] &
          oldVar <= input[[paste0("max", i)]]
      }
      f[i] <- input[[paste0("level", i)]]
      newVar[rows] <- input[[paste0("level", i)]]

    }

    newVar <- factor(newVar, levels = f)

    newVar
  })

  output$categoricalGraph <- renderPlot({
    req(discrete())

    ggplot(data.frame(var = discrete()),
           aes(var)) + geom_bar() +
      geom_text(stat = 'count',
                aes(label = ..count..),
                vjust = -.2)

  })

  # translate to SAS code
  observeEvent(input$sasCode, {
    req(input$tab, input$vars)

    if (input$tab == "Collapse") {
      req(input$freq1_rows_selected)

      tblNum <- max(which(sapply(1:2, function(i) {
        length(input[[paste0("freq", i, "_rows_selected")]]) > 1
      })))

      if (tblNum == 1) {
        pfmt <- paste0("PROC FORMAT;\nVALUE ",
                       input$newName, "F;\n")
        for (i in seq_len(nrow(freqTable2()))) {
          pfmt <- paste0(pfmt,
                         freqTable2()[i, "Value"], "=", freqTable2()[i, "Format"],
                         ";\n")
        }
        pfmt <- paste0(pfmt, "RUN;")

        SAScode <- paste0(
          "/* Code for constructing ",
          input$newName,
          " */;\n",
          input$newName,
          "=",
          input$vars,
          ";\n",
          "IF ",
          input$newName,
          " IN (",
          paste0(freqTable()[input$freq1_rows_selected, "Value"],
                 collapse = ", "),
          ") THEN ",
          input$newName,
          "=",
          min(freqTable()[input$freq1_rows_selected, "Value"]),
          ";\nLABEL ",
          input$newName,
          "=\"",
          input$newLabel,
          "\";\n",
          "FORMAT ",
          input$newName,
          " ",
          input$newName,
          "F.;\n"
        )
      } else if (tblNum == 2) {
        pfmt <- paste0("PROC FORMAT;\nVALUE ",
                       input$newName, "F;\n")
        for (i in seq_len(nrow(freqTable3()))) {
          pfmt <- paste0(pfmt,
                         freqTable3()[i, "Value"], "=", freqTable3()[i, "Format"],
                         ";\n")
        }
        pfmt <- paste0(pfmt, "RUN;\n\n")

        SAScode <- paste0(
          "/* Code for constructing ",
          input$newName,
          " */;\n",

          input$newName,
          "=",
          input$vars,
          ";\n",
          "IF ",
          input$newName,
          " IN (",
          paste0(freqTable()[input$freq1_rows_selected, "Value"],
                 collapse = ", "),
          ") THEN ",
          input$newName,
          "=",
          min(freqTable()[input$freq1_rows_selected, "Value"]),
          ";\nELSE IF ",
          input$newName,
          " IN (",
          paste0(freqTable2()[input$freq2_rows_selected, "Value"],
                 collapse = ", "),
          ") THEN ",
          input$newName,
          "=",
          min(freqTable2()[input$freq2_rows_selected, "Value"]),
          ";\nLABEL ",
          input$newName,
          "=\"",
          input$newLabel,
          "\";\n",
          "FORMAT ",
          input$newName,
          " ",
          input$newName,
          "F.;\n"
        )
      }

    } else if (input$tab == "Top Code") {
      pfmt <- paste0("PROC FORMAT;\nVALUE ",
                     input$newName,
                     "F;\n",
                     "LOW-HIGH='VALUE';\nRUN;\n")
      SAScode <-
        paste0(
          "/* Code for constructing ",
          input$newName,
          " */;\n",
          input$newName,
          "=",
          input$vars,
          ";\n",
          "IF ",
          input$newName,
          " LT 0 THEN ",
          input$newName,
          "=",
          input$newName,
          ";\n",
          "ELSE IF ",
          input$newName,
          " LT ",
          input$recoder[1],
          " THEN ",
          input$newName,
          "=",
          input$recoder[1],
          ";\n",
          "ELSE IF ",
          input$newName,
          " GT ",
          input$recoder[2],
          " THEN ",
          input$newName,
          "=",
          input$recoder[2],
          ";\n",
          "LABEL ",
          input$newName,
          "=\"",
          input$newLabel,
          "\"\n",
          "FORMAT ",
          input$newName,
          " ",
          input$newName,
          "F.;\n"
        )

    } else if (input$tab == "Group") {
      SAScode <-
        paste0("/* Code for constructing ",
               input$newName,
               " */;\n",
               input$newName,
               "=.;\n")

      pfmt <- paste0("PROC FORMAT;\nVALUE ",
                     input$newName, "F;\n")

      for (i in seq_len(counter$val)) {
        pfmt <- paste0(pfmt,
                       i, "=\"",
                       input[[paste0("level", i)]]
                       , "\";\n")
        if (i == 1) {
          SAScode <- paste0(
            SAScode,
            "IF ",
            input$vars,
            " GE ",
            input[[paste0("min", i)]],
            " AND ",
            input$vars,
            " LT ",
            input[[paste0("max", i)]],
            " THEN ",
            input$newName,
            "=",
            i,
            ";\n"
          )
        } else if (i == counter$val) {
          SAScode <- paste0(
            SAScode,
            "ELSE IF ",
            input$vars,
            " GE ",
            input[[paste0("min", i)]],
            " THEN ",
            input$newName,
            "=",
            i,
            ";\n"
          )
        } else {
          SAScode <- paste0(
            SAScode,
            "ELSE IF ",
            input$vars,
            " GE ",
            input[[paste0("min", i)]],
            " AND ",
            input$vars,
            " LT ",
            input[[paste0("max", i)]],
            " THEN ",
            input$newName,
            "=",
            i,
            ";\n"
          )
        }
      }

    }

    showModal(modalDialog(
      size = "l",
      h2("SAS Code:"),
      h4("Format Statement:"),
      HTML(gsub("\n", "<br>", pfmt)),
      h4("Data Statement:"),
      HTML(gsub("\n", "<br>", SAScode))
    ))
  })

  }