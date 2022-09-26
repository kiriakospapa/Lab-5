#'  find crime from both time and type
#' 
#'  find crime from both time and type
#' @param data get crime data
#' @param type crime type input
#' @param x month input
#' @import ggplot2
#' @import ggmap
#' @import shiny
#' @export 



#####Function outside in order to test#####

find_crime_from_time_and_type <- function(data,type,x){
  
  ###Check type input###
  if(type != "theft" & 
     type != "auto theft" & 
     type != "murder"& 
     type != "robbery"& 
     type != "aggravated assault"& 
     type != "burglary"){stop("Wrong Input, only: theft, auto theft, murder,robbery,aggravated assault,burglary.can be searched")}
  
  ###Check month input###
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
  
  data <- crime
  
  index_of_type <- which(data_of_the_month$offense == type)
  data_of_type <- data_of_the_month[index_of_type,]
  
  huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
  map <- ggmap::get_stamenmap(huston, maptype = "terrain", zoom=10)
  crime_map <- ggmap(map) + ggplot2::geom_jitter(data = data_of_type, ggplot2::aes(x=lon, y=lat),size = 0.005, color = "red")
  
  return(crime_map)
}

##### End of Function #####

crime <- ggmap::crime

##### Shiny #####


ui <- fluidPage(
  
  textInput(inputId = "crime_type",
            label = "crime_type", ),
  numericInput(inputId = "month", 
               label = "month", 
               value = 1, min = 1, max = 12, step = 1),
  
  plotOutput(outputId = "find_crime_from_time_and_type")
  
)

server <- function(input, output) {
  output$find_crime_from_time_and_type<- renderPlot(find_crime_from_time_and_type(crime,input$crime_type,input$month))
}
  

shinyApp(ui, server)

