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

# Function to return the date time variable in ISO8601 format, with the offset 
# added.
to_iso8601 <- function(datetime_str, offset) {
  
  # Convert the string to a datetime object
  datetime_var <- anytime(datetime_str)
  
  # Add the offset
  adjusted_datetime <- datetime_var + days(offset)
  
  # Convert to UTC timezone and format it in ISO8601 format with "Z" at the end
  formatted <- format(adjusted_datetime, 
                      format ="%Y-%m-%dT%H:%M:%SZ", 
                      tz = "UTC")
  
  return(formatted)
}


