library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("App B"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "var", "Select a variable to upcode",
        choices = c("continent_of_origin"),
        options = list(
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = 'input.var !== "" & input.var !== null',
        h2("Contintent of Origin"),
        fluidRow(
          column(width = 5,
          p("On the right are the set levels for the
            contitent of origin variable. Below is a table
            of answers respondents gave when they were asked their
            country of origin. Please categorize the responses to
            the correct continent. ")),
          column(width = 7,
          tableOutput("formats")
          )
        ),
        DT::dataTableOutput("responses")
      )
    )
  )
)
