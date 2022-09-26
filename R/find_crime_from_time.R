library(shiny)
library(ggmap)



#####Function outside in order to test#####

  find_crime_from_month<-function(data,x){

    if(x == 1){month <- "january"}
    else if(x == 2){month <- "february"}
    else if(x == 3){month <- "march"}
    else if(x == 4){month <- "april"}
    else if(x == 5){month <- "may"}
    else if(x == 6){month <- "june"}
    else if(x == 7){month <- "july"}
    else if(x == 8){month <- "august"}
    else if(x > 8 ){stop("Only data from January to August available")}


    index_of_month <- which(data$month == month)
    data_of_the_month <- data[index_of_month,]

    huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
    map <-ggmap::get_stamenmap(huston, maptype = "terrain", zoom=10)
    crime_map <-ggmap(map) + ggplot2::geom_jitter(data = data_of_the_month, ggplot2::aes(x=lon, y=lat),size = 0.0005, color = "red")

    return(crime_map)
  }

  ##### End of Function #####
  
  
  ##### Shiny #####
  
  ui <- fluidPage(
  
    numericInput(inputId = "month", label = "month", value = 1, min = 1, max = 12, step = 1),
  
    plotOutput(outputId = "find_crime_from_month")
  
  )
  
  server <- function(input, output) {
    output$find_crime_from_month<- renderPlot(find_crime_from_month(crime,input$month))
  }

shinyApp(ui, server)