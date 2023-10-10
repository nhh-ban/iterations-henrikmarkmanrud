# Function to transform dataset
transform_metadata_to_df <- function(stations_metadata) {
  
  # First extracting relevant data
  df <- 
    stations_metadata[[1]] %>% 
    map(as_tibble) %>% # Mapping the data as tibble
    list_rbind() %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    unnest_wider(location) %>% # using the unnest - function
    unnest_wider(latLon)
  
  return(df) # returning the data frame in the function 
  
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
  


