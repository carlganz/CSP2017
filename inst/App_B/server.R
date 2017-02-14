library(shiny)
library(DT)
library(dplyr)
library(stringdist)

server <- function(input, output, session) {
  session$onSessionEnded(stopApp)
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
      values = c(1:7),
      formats = c(
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
    readRDS("www/responses.rds")
  })

  pastUpcodes <- reactive({
    readRDS("www/pastUpcodes.rds")
  })

  output$responses <- DT::renderDataTable({
    req(input$var)
    session$sendCustomMessage("unbind-DT", "responses")
    compare <- upcodes()
    pastUpcodes <- pastUpcodes()
    compare[, "SimilarResponse"] <- NA
    compare[, "SimilarUpcode"] <- NA

    for (i in seq_len(nrow(compare))) {
      response <- compare[i, "answer"]

      comp <- stringdist(response,
                         pastUpcodes$answer)
      if (any(comp < 3)) {
        best <- which(comp == min(comp))
        compare[i, "SimilarResponse"] <- pastUpcodes$answer[best]
        compare[i, "SimilarUpcode"] <-
          c(
            "North America",
            "South America",
            "Asia",
            "Europe",
            "Africa",
            "Oceania",
            "Other"
          )[pastUpcodes$upcode[best]]
      }

    }

    data.frame(
      compare,
      inputUpcode =
        shinyInput(
          selectizeInput,
          nrow(compare),
          "upcodeVal",
          width = "120%",
          choices = c(
            "North America",
            "South America",
            "Asia",
            "Europe",
            "Africa",
            "Oceania",
            "Other"
          ),
          options =
            list(onInitialize = I('function() {
                                  this.setValue("")
  }'))
)
            )
  },
rownames = FALSE, selection = list(mode = "none"),
escape = c(-5), options = list(
  #autoWidth = TRUE,
  preDrawCallback = DT::JS('function() {
                           Shiny.unbindAll(this.api().table().node())
  }'),
    drawCallback = DT::JS(
      'function(settings) {
      Shiny.bindAll(this.api().table().node());
      l = this.api().table().data().length;
      for (i = 1;i<l+1; i++) {
      document.getElementById("upcodeVal"+i).value = "";
      }
}'
    )
  ))

  observeEvent(input$submit, {
    updateSelectizeInput(session, "var", selected = "")

    ### Would store upcodes in database if this wasn't a demo

    showModal(
      modalDialog(size = "s", easyClose = TRUE,
                  h2("Upcodes submitted!")
                  )
    )

  })

}