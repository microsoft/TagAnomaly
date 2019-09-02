library(ggplot2)
library(dplyr)
library(shiny)
#library(Cairo)   # For nicer ggplot2 output when deployed on Linux
#library(data.table)

cols <- c("mpg", "cyl", "disp", "hp", "wt", "am", "gear")
mtcars2 <- mtcars %>% select(cols)

ui <- fluidPage(
  fluidRow(
    column(width = 10,
           plotOutput("plot1", height = 300,
                      brush = "user_brush")
           
           
    ),
    column(width = 2, 
           br(),
           actionButton("add", "Add Points"),
           br(),
           br(),
           actionButton("delete", "Delete Points")
    )
  ),
  fluidRow(
    column(width = 6,
           h4("Brushed points"),
           dataTableOutput("summaryTable")
    )
  )
)


server <- function(input, output) {

  dataset <- reactive({
    mtcars2
  })
  
  selectedPoints <- reactive(value = data.frame(),label='selectedPoints')
  brushed <- reactive(brushedPoints(dataset(), input$user_brush))
  #brushed <- reactiveVal(value = brushedPoints(dataset(),input$user_brush),label='brushed')
  ### Using `observeEvent` to Add or Delete Points -------
  #points <- data.frame()
  #makeReactiveBinding("points")
  
  observeEvent(input$add, {
    selectedPoints(selectedPoints() %>% bind_rows(brushed()))
  })
  
  observeEvent(input$delete, {
    if (dim(selectedPoints())[1] > 0) {
      selectedPoints(selectedPoints()%>% anti_join(brushed()))
    }
  })
  ### ----------------------------------------------------
  
  output$plot1 <- renderPlot({
    p2 <- ggplot(dataset(), aes(wt, mpg)) + geom_point()
    if (dim(selectedPoints())[1] > 0) {
      p2 <- p2 + geom_point(aes(wt, mpg), data = selectedPoints(), color = "red")
    } 
    return(p2)
  })
  
  output$summaryTable <- DT::renderDataTable(expr = selectedPoints(), selection = 'single',server = F)
  
  
}

shinyApp(ui, server)

