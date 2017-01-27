library(shiny)
library(DT)
library(dplyr)
library(stringdist)


server <- function(input, output, session) {

  # helper function for making inputs
  # in datatable
  # stolen from Yihui somewhere
  shinyInput = function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }

  output$formats <- renderTable({
    req(input$var)
    data.frame(
      values = c(-1, 1:7),
      formats = c(
        "Inapplicable",
        "North America",
        "South America",
        "Asia",
        "Europe",
        "Africa",
        "Oceania",
        "Other"
      )
    )
  })

  upcodes <- reactive({
    readRDS("../Data/responses.rds")
  })

  pastUpcodes <- reactive({
    readRDS("../Data/pastUpcodes.rds")
  })

  output$responses <- DT::renderDataTable({
    req(input$var)

    compare <- upcodes()
    pastUpcodes <- pastUpcodes()
    compare[,"SimilarResponse"] <- NA
    compare[,"SimilarResponseUpcode"] <- NA

    for (i in seq_len(nrow(compare))) {
      response <- compare[i,"answer"]

      comp <- stringdist(response,
                         pastUpcodes$answer)
      if (any(comp < 3)) {
        best <- which(comp==min(comp))
        compare[i, "SimilarResponse"] <- pastUpcodes$answer[best]
        compare[i, "SimilarResponseUpcode"] <-
          pastUpcodes$upcode[best]
      }

    }

    data.frame(compare,
               inputUpcode =
                 shinyInput(numericInput, nrow(compare),
                            "upcodeVal", value = NULL)
                 )
  },
  rownames = FALSE, selection = list(mode="none"),
  escape = c(-5), options = list(
    autoWidth = TRUE,
    columnDefs = list(list(
      width = "20px", target = 3
    ))
  ))

}