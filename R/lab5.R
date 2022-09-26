textInputRow<-function (inputId, label, value = "", ...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small", ...))
}

map2 <- get_stamenmap()

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
  
  selectInput( "option", label = "Select what you want to see",
              choices = list("Winds in Huston", "Crimes in huston"),
              selected = NULL, width = "20%"),
  textInputRow(inputId="min_wind", label="Minimum of wind", value = 0, class="input-small"),
  textInputRow(inputId="max_wind", label="Maximum of wind", value = 100),
  actionButton("do", "Click me"),
  plotOutput(outputId="probs")
)

#================= CREATING BACKEND ======================

server <- function(input, output) {
  result <- ""
  observe({  result <- input$option
            print(result)
            output$probs <- renderPlot(
              
              # Change plot depending on the option selected
              { 
                if (input$option == "Winds in Huston"){
                    if(is.na(as.integer(input$min_wind)) | is.na(input$max_wind)){
                      stop("Insert integer not charachters")
                    }else{
                    wind_plot(as.integer(input$min_wind), as.integer(input$max_wind))
                    }
                }else{
                  ggmap(map2)
                }
                
            })
   })
}

shinyApp(ui, server)