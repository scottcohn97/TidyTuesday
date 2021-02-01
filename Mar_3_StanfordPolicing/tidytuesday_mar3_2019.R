# Scott Cohn
# Tidy Tuesday
# Mar 3, 2019

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-03-19

library(tidyverse)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(ggsci)
library(bbplot)
library(readr)


# Import Data -------------------------------------------------------------

combined_data <- readr::read_csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")

# Graphs ------------------------------------------------------------------

gg_stop_rate <- combined_data %>% 
  group_by(driver_race) %>% 
  dplyr::summarize(mean_stop_rate = mean(stop_rate, na.rm = TRUE)) %>% 
  ggplot(aes(x = driver_race,
             y = mean_stop_rate,
             fill = driver_race)
         ) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  bbc_style() +
  scale_fill_d3() +
  labs(
    title = "Traffic Stop Rate by Race",
    subtitle = "Stop rate (stop = police stop of a vehicle) (%)"
  ) 

finalise_plot(plot_name = gg_stop_rate,
              source = "Source: The Stanford Open Policing Project",
              save_filepath = "stop_rate.pdf",
              width_pixels = 840,
              height_pixels = 550)

gg_hit_rate <- combined_data %>% 
  group_by(driver_race, state) %>% 
  filter(state == "CO") %>% 
  dplyr::summarize(mean_hit_rate = mean(hit_rate, na.rm = TRUE)) %>% 
  ggplot(aes(x = driver_race,
             y = mean_hit_rate,
             fill = driver_race
            )
  ) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 1, colour = "#333333") +
  scale_fill_d3() +
  bbc_style() +
  labs(
    title = "Traffic Stop Hit Rate by Race",
    subtitle = "Hit rate (%): the proportion of searches that successfully turn up contraband"
  ) 

finalise_plot(plot_name = gg_hit_rate,
              source = "Source: The Stanford Open Policing Project",
              save_filepath = "hit_rate.pdf",
              width_pixels = 840,
              height_pixels = 550)

