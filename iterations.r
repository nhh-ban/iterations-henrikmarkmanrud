library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)

#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

selected_station <- stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) # Sampling one station from the data frame

name <- selected_station$name # Defining the station to a variable to be able 
# to add legend in the ggplot

vol_data <- vol_qry(
  id = selected_station$id,
  from = to_iso8601(selected_station$latestData, -4),
  to = to_iso8601(selected_station$latestData, 0)
)

# Use vol_data defined earlier to make the ggplot
vol_data %>% 
  GQL(.url = configs$vegvesen_url) %>%
  transform_volumes() %>% # Using the transform from data_transformations
  mutate(name = name) %>% # Adding the name for the station
  ggplot(aes(x = from, y = volume, color = name)) + 
  geom_line() + # Adding a line to the plot
  scale_color_manual(values = "blue", name = "") + # Manually adding the legend
  theme_classic() + 
  xlab("Date") + # Making the axis labels cleaner
  ylab("Volume")



