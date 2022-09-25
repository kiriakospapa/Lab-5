library(shiny)
library("ggmap")

ui <- fluidPage(
  
  numericInput(inputId = "left", 
               label = "left", 
               value = 1, min = 1, max = 180, step = 0.000001),
  numericInput(inputId = "right", 
               label = "right", 
               value = 1, min = 1, max = 180, step = 0.000001),
  numericInput(inputId = "top", 
               label = "top", 
               value = 1, min = 1, max = 180, step = 0.000001),
  numericInput(inputId = "bottom", 
               label = "bottom", 
               value = 1, min = 1, max = 180, step = 0.000001),
  plotOutput(outputId = "map_in_data")
  
)

server <- function(input, output) {
  
  output$map_in_data <- renderPlot({
    
    if(input$left < -99.50555 |input$right > -91.94627 | input$top > 37.33690| input$bottom < 27.50711){stop("Not in Data")}
    else{
    us <- c(left = input$left, bottom = input$bottom , right = input$right, top = input$top)
    get_stamenmap(us, zoom = 5, maptype = "toner-lite") %>% ggmap()
    
    }
  })
  
}

shinyApp(ui, server)