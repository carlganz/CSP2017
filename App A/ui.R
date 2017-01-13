library(shiny)
library(DT)
library(rintrojs)
library(plotly)

ui <- fluidPage(introjsUI(),
                includeCSS("www/style.css"),
                titlePanel("App A"),
                sidebarLayout(sidebarPanel(
                  selectizeInput("vars", "Select a CHIS variable",
                                 choices = c())
                ),
                mainPanel(
                  tabsetPanel(
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
                      h2("Warning: Use first tab for categorical variables")
                    ),
                    plotlyOutput("beforeRecode"),
                    uiOutput("mySlider"),
                    plotlyOutput("afterRecode")
                  ),
                  tabPanel(
                    title = "Group",
                    conditionalPanel(
                      condition = "output.varType & output.varType != null",
                      h2("Warning: Use first tab for categorical variables")
                    ),
                    plotlyOutput("continousGraph"),
                    plotlyOutput("categoricalGraph")
                  )
                  ),
                  conditionalPanel(
                    condition = 'input.vars != null & input.vars != ""',
                    textInput("newName", "Name new variable:"),
                    textInput("newLabel", "Give new label:")
                  )
                )))
