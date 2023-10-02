
# Function to transform dataset
transform_metadata_to_df <- function(stations_metadata) {
  
  # First extracting relevant data
  df <- tibble(data = stations_metadata$trafficRegistrationPoints) %>% 
    
    transmute(
      id = map_chr(data, ~ .x$id),
      name = map_chr(data, ~ .x$name),
      latestData = map(data, ~ .x$latestData$volumeByHour), 
      lat = map_dbl(data, ~ .x$location$coordinates$latLon$lat),
      lon = map_dbl(data, ~ .x$location$coordinates$latLon$lon)
      ) %>% 
    
    mutate(
      latestData = ymd_hms(latestData, tz="UTC")
    )
  
  return(df)
  
}

