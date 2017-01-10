library(shiny)
library(DT)
library(rintrojs)

ui <- fluidPage(
  introjsUI(),
  titlePanel("App C"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel()
  )
)
