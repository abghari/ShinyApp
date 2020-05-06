library(shiny)


ui <- fluidPage(

  sidebarPanel(),
  
  mainPanel (textOutput("date"),
             plotOutput("demand")
  )
)