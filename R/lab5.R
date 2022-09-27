textInputRow<-function (inputId, label, value = "", ...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small", ...))
}

map2 <- get_stamenmap()

#================= FIND CRIME FROM MONTH =================

find_crime_from_month<-function(data,x){
  
  if(x == 1){month <- "january"}
  else if(x == 2){month <- "february"}
  else if(x == 3){month <- "march"}
  else if(x == 4){month <- "april"}
  else if(x == 5){month <- "may"}
  else if(x == 6){month <- "june"}
  else if(x == 7){month <- "july"}
  else if(x == 8){month <- "august"}
  else if(x > 8 | x <= 0 ){stop("Month should be an integer from 1 to 8")}
  
  
  index_of_month <- which(data$month == month)
  data_of_the_month <- data[index_of_month,]
  
  huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
  map <-ggmap::get_stamenmap(huston, maptype = "terrain", zoom=10)
  crime_map <-ggmap(map) + ggplot2::geom_jitter(data = data_of_the_month, ggplot2::aes(x=lon, y=lat),size = 0.0005, color = "red")
  
  return(crime_map)
}


#============ FIND CRIME FROM TIME AND TYPE =============

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
  else if(x > 8 | x <= 0 ){stop("Month should be an integer from 1 to 8")}
  
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

#=========== CREATE A NEW TABLE FOR WIND DATA ===========

updated_wind <- wind %>% rowwise() %>% 
  mutate(lon2 = lon + delta_lon) %>%
  mutate(lat2 = lat + delta_lat)

#== FUNCTION FOR CREATING THE MAP RAGARDING THE INPUTS===

wind_plot <- function(min, max){
  updated_wind <- updated_wind %>%
    filter(
      -95.4 <= lon & lon <= -95,
      29.6 <= lat & lat <= 29.8,
      min <= spd & spd <= max
    )
  huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
  map <-get_stamenmap(huston, maptype = "terrain", zoom=10)
  plot1 <- ggmap(map) + geom_segment(data = updated_wind, aes(x=lon, y=lat, xend=lon2, yend=lat2, color=spd), alpha=0.8, size=2) 
  plot(plot1)
  
  
}


#=================== DESING THE UI =======================

ui <- fluidPage(
  
  useShinyjs(),
  selectInput( "option", label = "Select what you want to see",
              choices = list("Winds in Huston", "Crimes in huston by month and time", "Crimes in huston by month", "Crimes in huston by time"),
              selected = NULL, width = "20%"),
  textInputRow(inputId="min_wind", label="Minimum of wind", value = 0, class="input-small"),
  textInputRow(inputId="max_wind", label="Maximum of wind", value = 100),
  plotOutput(outputId="probs")
)

#================= CREATING BACKEND ======================

server <- function(input, output, session) {
  result <- ""
  observe({  result <- input$option
            print(result)
            output$probs <- renderPlot(
              
              # Change plot depending on the option selected
              { 
                if (input$option == "Winds in Huston"){
                  shinyjs::show("max_wind")
                  updateActionButton(session, "min_wind",
                                     label = "Minumum of wind")
                  updateActionButton(session, "max_wind",
                                     label = "Maximum of wind")
                  
                  # updateNumericInput(session, "min_wind", value = "0")
                  # updateNumericInput(session, "max_wind", value = "100")
                  
                   if(input$min_wind == ""| input$max_wind == ""){
                        
                     stop("Insert a value")
                   } 
                  else if (is.na(as.integer(input$min_wind)) | is.na(as.integer(input$max_wind))){
                      
                    stop("Insert integer not charachters")
                    }
                  else{
                    
                    wind_plot(as.integer(input$min_wind), as.integer(input$max_wind))
                    }
                }
                else if(input$option == "Crimes in huston by month and time"){
                  
                  shinyjs::show("max_wind")
                  
                  
                  updateActionButton(session, "min_wind",label = "Month")

                  updateActionButton(session, "max_wind",
                                     label = "Type of crime")
                  
                  if(as.double(input$min_wind) %% 1 == 0)
                  {
                 
                    plot(find_crime_from_time_and_type(crime, input$max_wind, as.integer(input$min_wind)))
                  }
                  else{
                    
                    stop("Month should be an integer from 1 to 8" )
                  }
                }
                else if(input$option == "Crimes in huston by month"){

                  shinyjs::hide("max_wind")
                  updateActionButton(session, "max_wind",
                                     label = "")
                  updateActionButton(session, "min_wind",
                                     label = "Month")
                  if(as.double(input$min_wind) %% 1 == 0)
                  {
                    
                    find_crime_from_month(crime, as.integer(input$min_wind))
                  }
                  else{
                    
                    stop("Month should be an integer from 1 to 8" )
                  }
                }
                
            })
   })
}

shinyApp(ui, server)