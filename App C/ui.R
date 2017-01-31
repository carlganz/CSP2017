library(shiny)
library(DT)

ui <- fluidPage(titlePanel("App C"),
                sidebarLayout(
                  sidebarPanel(
                    tabsetPanel(id = "which",
                      tabPanel(
                        "Download",
                        selectizeInput(
                          "year",
                          "Choose year",
                          2011:2014, multiple = TRUE,
                          options = list(
                            placeholder = "Choose year",
                            onInitialize = I('function() {this.setValue("")}')
                          )
                        ),
                        selectizeInput(
                          "age",
                          "Choose age group",
                          c("ADULT", "TEEN", "CHILD"),
                          multiple = TRUE,
                          options = list(
                            placeholder = "Choose year",
                            onInitialize = I('function() {this.setValue("")}')
                          )
                        ),
                        conditionalPanel(
                          condition = 'input.year != "" & input.year != null
                      & input.age != "" & input.age != null',
                          downloadButton("download", "Download Excel DB")
                        )
                      ),
                      tabPanel(
                        "Upload",
                        fileInput("uploader",
                                  "Upload Excel file with changes",
                                  accept = c(".xlsx",".XLSX"))
                      )
                    )
                  ),
                  mainPanel(
                    conditionalPanel(
                      condition = "input.which=='Download'",
                      DT::dataTableOutput("db")
                    ),
                    conditionalPanel(
                      condition = "input.which=='Upload'",
                      DT::dataTableOutput("changes")
                    )
                    )
                  ))
