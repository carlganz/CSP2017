library(shiny)
library(DT)
library(rintrojs)

ui <- fluidPage(
  introjsUI(),
  titlePanel("App A"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel()
  )
)
