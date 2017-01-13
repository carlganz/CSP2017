library(shiny)
library(DT)
library(rintrojs)
library(dplyr)
library(ggplot2)
library(plotly)


server <- function(input, output, session) {
  # load data
  chis <- readRDS("../Data/chis.rds")

  output$varType <- reactive({
    req(input$vars)
    class(chis[[input$vars]]) %in% c("character",
                                     "factor")
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
                    value = paste0(
                      attr(chis[[input$vars]], "label"),
                                   " (PUF Recode)"))
  })

  # first tab
  output$freq1 <- DT::renderDataTable({
    req(input$vars)
    as.data.frame(table(chis[[input$vars]]))
  }, rownames = FALSE, selection = list(target = "row"),
  options = list(paging = FALSE, bInfo = 0,
                 bSort = 0, bFilter = 0,
                 rowCallback = DT::JS(
                   'function(row, data) {
                   if (parseFloat(data[1])< 100) {
                   $("td", row).css("color", "red").
                   css("font-weight", "bold");
                    }
                   }'
                 )))

  observe({
    updateSelectizeInput(session, "vars",
                         choices = names(chis),
                         server = TRUE)
  })

  # second tab

  output$beforeRecode <- renderPlotly({
    req(input$vars)
    plot <- ggplot(chis,
                   aes_string(input$vars)) + geom_histogram() +
      ggtitle(paste0(input$vars," before recoding")) +
      geom_text(stat = 'bin', aes(label = ..count..), vjust = -.2)

      ggplotly(plot, tooltip = c())
  })

  output$mySlider <- renderUI({
    req(input$vars)
    sliderInput("recoder",
                "Select new min, and max values",
                min = min(chis[[input$vars]]),
                max = max(chis[[input$vars]]),
                value = c(min(chis[[input$vars]]),
                          max(chis[[input$vars]]))
    )
  })

  output$afterRecode <- renderPlotly({
    req(input$vars, input$recoder)

    chis2 <- chis

    chis2[chis2[[input$vars]]< input$recoder[1],input$vars] <- input$recoder[1]
    chis2[chis2[[input$vars]]> input$recoder[2],input$vars] <- input$recoder[2]

    plot <- ggplot(chis2,
                   aes_string(input$vars)) + geom_histogram() +
      ggtitle(paste0(input$vars," after recoding")) +
      geom_text(stat = 'bin', aes(label = ..count..), vjust = -.2)

    ggplotly(plot, tooltip = c())
  })

}