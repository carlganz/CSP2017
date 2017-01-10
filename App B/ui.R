library(shiny)
library(DT)
library(rintrojs)

ui <- fluidPage(
  introjsUI(),
  titlePanel("App B"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel()
  )
)
