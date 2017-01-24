library(shiny)
library(DT)
library(rintrojs)
library(plotly)

ui <- fluidPage(introjsUI(),
                includeCSS("www/style.css"),
                titlePanel("App A"),
                sidebarLayout(sidebarPanel( width = 3,
                  selectizeInput("vars", "Select a CHIS variable",
                                 choices = c("CA6",
                                             "SRAGE"),
                                 options = list(
                                   placeholder = "Select a CHIS variable",
                                   onInitialize = I('function() {this.setValue("");}')
                                 )),
                  conditionalPanel(
                    condition = 'input.tab==="Group"',
                    fluidRow(
                      h3("Change # of levels"),
                        actionButton("removeLevel", "Remove"),
                        actionButton("addLevel","Add")
                    )
                  )
                ),
                mainPanel(
                  tabsetPanel(
                    id = "tab",
                    tabPanel(
                      title = "Collapse",
                      conditionalPanel(
                        condition = "!output.varType & output.varType != null",
                        h2("Warning: Use a different tab for continuous variables",
                           class = "warning")
                      ),
                    DT::dataTableOutput("freq1"),
                    conditionalPanel(
                      condition = 'input.freq1_rows_selected != "" &
                      input.freq1_rows_selected != null',
                      DT::dataTableOutput("freq2"),
                      conditionalPanel(
                        condition = 'input.freq2_rows_selected != ""',
                        DT::dataTableOutput("freq3"),
                        conditionalPanel(condition = 'input.freq3_rows_selected != ""',
                                         DT::dataTableOutput("freq4"))
                      )
                    )
                  ),
                  tabPanel(
                    title = "Top Code",
                    conditionalPanel(
                      condition = "output.varType & output.varType != null",
                      h2("Warning: Use first tab for categorical variables",
                         class = "warning")
                    ),
                    plotOutput("beforeRecode"),
                    uiOutput("mySlider"),
                    plotOutput("afterRecode")
                  ),
                  tabPanel(
                    title = "Group",
                    conditionalPanel(
                      condition = "output.varType & output.varType != null",
                      h2("Warning: Use first tab for categorical variables",
                         class = "warning")
                    ),
                    uiOutput("sliders"),
                    plotOutput("continousGraph"),
                    plotOutput("categoricalGraph")
                  )
                  ),
                  conditionalPanel(
                    condition = 'input.vars != null & input.vars != ""',
                    textInput("newName", "Name new variable:"),
                    textInput("newLabel", "Give new label:"),
                    actionButton("sasCode", "View SAS code")
                  )
                )))
