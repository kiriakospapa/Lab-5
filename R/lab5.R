
#=========== CREATE A NEW TABLE FOR WIND DATA ===========

updated_wind <- wind %>% rowwise() %>% # Create 2 new columns with the coordinates of the second point of the winds
  mutate(lon2 = lon + delta_lon) %>%
  mutate(lat2 = lat + delta_lat)

updated_wind <- updated_wind %>%
  filter(
    -95.4 <= lon & lon <= -95,
    29.6 <= lat & lat <= 29.8
  )

#=========== CREATE A NEW TABLE FOR WIND DATA ===========

huston <- c(left= -95.4, bottom = 29.6, right = -95, top = 29.8)
map <-get_stamenmap(huston, maptype = "terrain", zoom=10)
plot <- ggmap(map) + geom_segment(data = updated_wind, aes(x=lon, y=lat, xend=lon2, yend=lat2, color=spd), alpha=0.8, size=2) 
plot

