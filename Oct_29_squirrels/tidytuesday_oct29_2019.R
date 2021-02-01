# Scott Cohn
# Tidy Tuesday
# Oct 29

#https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-10-29

library(tidyverse)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(ggsci)
library(bbplot)
library(readr)

# Import data
nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

# Map ---------------------------------------------------------------------

# getting the map
center_lon_NYC <- mean(nyc_squirrels$long)
center_lat_NYC <- mean(nyc_squirrels$lat)

ggmap::register_google(key = "AIzaSyA0W1yJjxK6kQvjCuq43_s0K2A2nNcqve4") # Add API Key before running
mapNYC <- get_map(location = c(long = center_lon_NYC, 
                               lat = center_lat_NYC
                               ), 
                  zoom = 14,
                  maptype = "terrain", 
                  color = "bw", 
                  scale = 2
                  )

# All squirrels by age
squirrel_filter <- subset(nyc_squirrels, 
                          age %in% c("Adult", "Juvenile")
                          )

nyc_squirrels_filter <- nyc_squirrels %>% 
  filter(age %in% c("Adult", "Juvenile")) 

nyc_squirrels_map <-  ggmap(mapNYC) +
  geom_jitter(data = nyc_squirrels_filter , 
              aes(x = long, 
                  y = lat, 
                  alpha = 0.8, 
                  color = age
                  ), 
              size = .5) +
  scale_color_d3() +
  guides(alpha = FALSE, size = FALSE) +
  labs(color = "Squirrels by Age", x = "Longitude", y = "Lattitude")

# Noises ------------------------------------------------------------------

squirrel_actions <- c("Approaches", "Indifferent", "Kuks", "Moans", "Quass", 
                      "Runs From", "Tail Flags", "Tail Twitches")

squirrel_activities <- nyc_squirrels %>%
  gather(activity, value, kuks:other_interactions) %>%
  filter(value == "TRUE") %>%
  select(unique_squirrel_id, location, activity) %>%
  ggplot(aes(
    x = activity,
    fill = activity)
    ) +
  geom_bar() +
  scale_fill_d3(labels = squirrel_actions) +
  geom_hline(yintercept = 0, size = 1, color = "#333333") +
  bbc_style() +
  labs(
    title = "Do Squirrels Fear Us?",
    subtitle = "Squirrel Observations"
    ) + 
  scale_x_discrete(labels = squirrel_actions) 

finalise_plot(plot_name = squirrel_activities,
              source = "Source: NYC Squirrel Census",
              save_filepath = "squirrel_activities.pdf",
              width_pixels = 840,
              height_pixels = 550)
