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

# Function that transforms the json-return from the API to a data frame 
# that can be used for plotting

transform_volumes <- function(data_json) {
  data_json$trafficData$volume$byHour$edges %>%
    tibble::tibble(data = .) %>%
    tidyr::unnest_wider(data) %>%
    tidyr::unnest_wider(node) %>%
    tidyr::unnest_wider(total) %>%
    tidyr::unnest_wider(volumeNumbers) %>%
    dplyr::mutate(
      from = lubridate::ymd_hms(from, quiet = TRUE),
      to = lubridate::ymd_hms(to, quiet = TRUE),
      volume = as.numeric(volume)
    )
}
  


