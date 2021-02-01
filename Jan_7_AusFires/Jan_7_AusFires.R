# Scott Cohn
# Tidy Tuesday
# Jan 7

#https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-29

library(tidyverse)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(readr)
library(here) # For cleaning script (not shown)
library(sf) # For map
library(mapview) # For map

# Import data

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# Mapping NSW Current Incidents in R -------------------------------------------

#' Current Incidents Feed (GeoJSON)
#' This feed contains a list of current incidents from the NSW RFS, 
#' and includes location data and Major Fire Update summary information where available. 
#' Click through from the feed to the NSW RFS website for full details of the update. 
#' GeoJSON is a lightweight data standard that has emerged to support the sharing of 
#' information with location or geospatial data. 
#' It is widely supported by modern applications and mobile devices.

url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"

fires <- st_read(url)

fires

mapview(fires)

#' Hacky way to get rid of points within geometry collections
fire_poly <- fires %>% 
  st_buffer(dist = 0) %>% 
  st_union(by_feature = TRUE)

mapview(fire_poly)

fires %>% 
  mutate(pubdate = as.character(pubDate),
         pubdate = as.Date(pubdate))

read_file_list <- list.files(here::here("2020", "2020-01-07")) %>% 
  .[str_detect(., "tmax|tmin")]

read_clean_temp_data <- function(file_name){
  
  temp_descrip <- if_else(str_detect(file_name, "min"), "min", "max")
  
  read_csv(here::here("2020", "2020-01-07", file_name)) %>% 
    janitor::clean_names() %>% 
    fill(site_name, site_number) %>% 
    filter(!is.na(date)) %>% 
    rename(temperature = contains("temp")) %>% 
    mutate(temp_type = temp_descrip)  %>% 
    mutate(city_name = word(site_name, 1)) %>%  
    select(city_name, date, temperature, temp_type, site_name)
}

# Explore -----------------------------------------------------------------

temperature <- temperature %>%  
  rename(temp = temperature) 

# Plot temp over time by city
temperature %>% 
  filter(city_name == "PERTH") %>% 
  ggplot() +
  geom_point(aes(
    x = date,
    y = temp,
    color = temp_type
    )
  ) + 
  theme_minimal()

temperature %>% 
  filter(city_name == "SYDNEY") %>% 
  ggplot() +
  geom_point(aes(
    x = date,
    y = temp,
    color = temp_type
  )
  ) + 
  theme_minimal()

temperature %>% 
  filter(city_name == "MELBOURNE") %>% 
  ggplot() +
  geom_point(aes(
    x = date,
    y = temp,
    color = temp_type
  )
  ) + 
  theme_minimal()


# Plot  rainfall over time by city
rainfall <- rainfall %>%  
  rename(rfall = rainfall) 

rainfall %>% 
  filter(city_name == "Perth") %>% 
  #filter(month == 01) %>% #  & day == 01) %>% 
  drop_na(rfall) %>% 
  ggplot() +
  geom_point(aes(
    x = year,
    y = rfall
  )
  ) + 
  theme_minimal()
