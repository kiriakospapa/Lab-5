data("crime")
test_that("lenreg rejects errounous input", {

  expect_error(find_crime_from_type(crime, "thft"))
})





# library(shinytest)
# library(testthat)
# 
# 
# #open shiny app
# app <- ShinyDriver$new("...")
# 
# test_that("app gets expected output", {
#   #set numeric input
#   x <-app$setInputs(crime_type = "thft")
#   #get output
# 
# 
# })
# 
# #stop shiny app
# app$stop()