#'  find crime from type
#' 
#'  find crime from type
#' @import ggplot2
#' @import ggmap
#' @import shiny
#' @export 


#####Function outside in order to test#####

  find_crime_from_type <- function(data,type){
  
    if(type != "theft" & 
       type != "auto theft" & 
      type != "murder"& 
       type != "robbery"& 
      type != "aggravated assault"& 
       type != "burglary"){stop("Wrong Input, only: theft, auto theft, murder,robbery,aggravated assault,burglary.can be searched")}
  
  
   index_of_type <- which(data$offense == as.character(type))
   data_of_type <- data[index_of_type,]
  
   huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
   map <- ggmap::get_stamenmap(huston, maptype = "terrain", zoom=10)
   crime_map<- ggmap::ggmap(map) + ggplot2::geom_jitter(data = data_of_type, ggplot2::aes(x=lon, y=lat),size = 0.005, color = "red")
  
   return(crime_map)
  }


  ##### End of Function #####


  ##### Shiny #####
  ui <- shiny::fluidPage(
  
    shiny::textInput(inputId = "crime_type",label = "crime_type" ),
  
    shiny::plotOutput(outputId = "find_crime_from_type")
  
  )

  server <- function(input, output) {
    output$find_crime_from_type<- renderPlot(find_crime_from_type(crime,input$crime_type))
  }

  shiny::shinyApp(ui=ui, server=server)





