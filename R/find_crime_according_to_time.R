

  find_crime_from_month <-function(data, x){
    
    if(x == 1){month <- "january"}
    else if(x == 2){month <- "february"}
    else if(x == 3){month <- "march"}
    else if(x == 4){month <- "april"}
    else if(x == 5){month <- "may"}
    else if(x == 6){month <- "june"}
    else if(x == 7){month <- "july"}
    else if(x == 8){month <- "august"}
    else if(x > 8 ){stop("Not in data")}
    
    index_of_month <- which(data$month == month)
    data_of_the_month <- data[index_of_month,]
   
    `%notin%` <- function(lhs, rhs) !(lhs %in% rhs)
    
    month_crimes <- crime %>%
      filter(
        month %notin% c(month),
        -95.39681 <= lon & lon <= -95.34188,
        29.73631 <= lat & lat <=  29.78400
      ) %>%
      mutate(
        month = fct_drop(month),
        month = fct_relevel(month, c("february", "march", "april", "may","june","july","august",))
      )

    qmplot(-95.43739, 29.67790, data = data_of_the_month, zoom = 14, maptype = "toner-lite", color = I("red"))
    
    
   return ()
  
  }
  
  
  