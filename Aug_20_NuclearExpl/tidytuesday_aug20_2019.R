# Scott Cohn
# Tidy Tuesday
# Mar 3, 2019

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-08-20

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(ggsci)
library(bbplot)
library(readr)
library(countrycode)


# Import Data -------------------------------------------------------------

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")

# Add continent
nuclear_explosions$continent <- countrycode(sourcevar = nuclear_explosions$country,
                            origin = "country.name",
                            destination = "continent")

# nuclear_explosions %>% mutate(continent = if_else(country == "PAKIST", "Asia"))

# Map ---------------------------------------------------------------------
ggmap::register_google(key = "AIzaSyA0W1yJjxK6kQvjCuq43_s0K2A2nNcqve4") # Add API Key before running
# North America
# nuclear_NA <- nuclear_explosions %>% 
#   group_by(continent) %>% 
#   filter(continent == "North America")
# 
# center_lon_NA <- mean(nuclear_NA$latitude)
# center_lat_NA <- mean(nuclear_NA$latitude)

mapNA <- get_map(location = c(long = center_lon_NA, 
                              lat = center_lat_NA
                 ), 
                 zoom = 3,
                 maptype = "terrain", 
                 color = "bw", 
                 scale = 2
                )

# South America
# nuclear_SA <- nuclear_explosions %>% 
#   group_by(continent) %>% 
#   filter(continent == "South America")
# 
# center_lon_SA <- mean(nuclear_SA$longitude)
# center_lat_SA <- mean(nuclear_SA$latitude)

# Europe
nuclear_Eur <- nuclear_explosions %>% 
  group_by(continent) %>% 
  filter(continent == "Europe")

center_lon_Eur <- mean(nuclear_Eur$longitude)
center_lat_Eur <- mean(nuclear_Eur$latitude)

mapEur <- get_map(
  location = c(long = center_lon_Eur,
               lat = center_lat_Eur),
  zoom = 3,
  maptype = "terrain",
  color = "bw",
  scale = 2
)

nuclear_mapEur <-  ggmap(mapEur) +
  geom_point(data = nuclear_Eur , 
              aes(x = longitude, 
                  y = latitude, 
                  alpha = 1, 
                  color = purpose,
                  size = yield_upper
              )) +
  scale_color_d3("category20") +
  guides(alpha = FALSE, size = FALSE) +
  labs(color = "Purpose", x = "Longitude", y = "Lattitude")

# Asia
nuclear_Asia <- nuclear_explosions %>% 
  group_by(continent) %>% 
  filter(continent == "Asia")

center_lon_Asia <- mean(nuclear_Asia$longitude)
center_lat_Asia <- mean(nuclear_Asia$latitude)

mapAsia <- get_map(
  location = c(long = center_lon_Asia,
               lat = center_lat_Asia),
  zoom = 3,
  maptype = "terrain",
  color = "bw",
  scale = 2
)

nuclear_mapAsia <-  ggmap(mapAsia) +
  geom_point(data = nuclear_Asia , 
             aes(x = longitude, 
                 y = latitude, 
                 alpha = 1, 
                 color = purpose,
                 size = yield_upper
             )) +
  scale_color_d3("category20") +
  guides(alpha = FALSE, size = FALSE) +
  labs(color = "Purpose", x = "Longitude", y = "Lattitude")


# Graph -------------------------------------------------------------------

nuc_hist <- nuclear_explosions %>% 
  group_by(country) %>% 
  ggplot(aes(
    x = year, 
    fill = country),
  ) + 
  geom_histogram(binwidth = 5, position = "dodge") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() +
  scale_fill_d3() +
  scale_x_continuous(limits = c(1945, 2000),
                     breaks = seq(1945, 2000, by = 5)) +
  labs(
    title = "Nuclear Explosions",
    subtitle = "Frequency of Explosions by Country"
  ) 

finalise_plot(plot_name = nuc_hist,
              source = "Source: Stockholm International Peace Research Institute",
              save_filepath = "nuc_hist.pdf",
              width_pixels = 840,
              height_pixels = 550)

