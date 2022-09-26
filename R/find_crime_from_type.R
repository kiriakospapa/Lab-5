#'  Linear Regression
#' 
#'  Run Linear Regression 
#' @import ggplot2
#' @import ggmap
#' @import shiny
#' @export 

crime <- ggmap::crime

ui <- shiny::fluidPage(
  
  shiny::textInput(inputId = "crime_type",
                   label = "crime_type" ),
  # if(input$crime_type!=	 murder){stop()},
  
  shiny::plotOutput(outputId = "find_crime_from_type")
  
)

server <- function(input, output) {
  
  output$find_crime_from_type <- renderPlot({
    
    data <- crime
    type <-input$crime_type
    if(type!=	 "murder" & type != "robbery"& type != "aggravated assault" & type!="auto theft"& type!="theft"){print("Wrong Type")}
    else{
    index_of_type <- which(data$offense == input$crime_type)
    data_of_type <- data[index_of_type,]
    
    huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
    map <- ggmap::get_stamenmap(huston, maptype = "terrain", zoom=7)
    ggmap::ggmap(map) + ggplot2::geom_jitter(data = data_of_type, ggplot2::aes(x=lon, y=lat),size = 0.005, color = "red")
    }
    
  })
  
}

shiny::shinyApp(ui=ui, server=server)

find_crime_from_type <- function(data,type){



      index_of_month <- which(data$offense == as.character(type))
      data_of_the_month <- data[index_of_month,]

      huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
      map <- ggmap::get_stamenmap(huston, maptype = "terrain", zoom=7)
      ggmap::ggmap(map) + ggplot2::geom_jitter(data = data_of_type, ggplot2::aes(x=lon, y=lat),size = 0.005, color = "red")

    return(crime_map)
    }
