data("crime")

#1 Check wrong type name in find_crime_from_type 
  test_that("find_crime_from_type rejects wrong type name", {

   expect_error(find_crime_from_type(crime, "shoplifting"))
  })
  
  
#2 Check wrong month input(bigger than 8) in find_crime_from_month
  test_that("find_crime_from_month rejects month not include in data", {
  
    expect_error(find_crime_from_month(crime, 9))
  })

  
#3 Check wrong month input(with decimals)  in find_crime_from_month
  test_that("find_crime_from_month rejects input with decimals", {
  
    expect_error(find_crime_from_month(crime, 7.5))
  })
  
  
#4 Check wrong type name in find_crime_from_time_and_type
  test_that("find_crime_from_type rejects wrong type name", {
  
    expect_error(find_crime_from_time_and_type(crime, "shoplifting",2))
  })

  
#5 Check wrong month input(bigger than 8) in find_crime_from_time_and_type
  test_that("find_crime_from_month rejects month not include in data", {
    expect_error(find_crime_from_time_and_type(crime, "theft",9.5))
  })

#6 Check wrong month input(with decimals) in find_crime_from_time_and_type
  test_that("find_crime_from_type rejects month with decimals", {
  
    expect_error(find_crime_from_time_and_type(crime, "shoplifiting",5.5))
  })
  
#7 Check wrong month input for every inputs in find_crime_from_time_and_type
  test_that("find_crime_from_type rejects errounous input", {
    
    expect_error(find_crime_from_time_and_type(crime, "shoplifiting",5.5))
  })  