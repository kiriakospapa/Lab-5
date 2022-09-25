library(shiny)
library("ggmap")

ui <- fluidPage(
  
  textInput(inputId = "crime_type", 
               label = "crime_type", ),
  
  plotOutput(outputId = "find_crime_from_month")
  
)

server <- function(input, output) {
  
  output$find_crime_from_month <- renderPlot({
    
    x <- input$crime_type
    data <- crime
    
    
    index_of_month <- which(data$month == as.character(x))
    data_of_the_month <- data[index_of_month,]
    
    huston <- c(left= -95.4, bottom = 29.6, right = -95.1, top = 29.8)
    map <-get_stamenmap(huston, maptype = "terrain", zoom=10)
    ggmap(map) + geom_jitter(data = data_of_the_month, aes(x=lon, y=lat),size = 0.0005, color = "red")
    
    
  })
  
}

shinyApp(ui, server)