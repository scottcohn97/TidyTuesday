# Scott Cohn
# Tidy Tuesday
# Jun 4, 2019

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-06-04
   
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggsci)
library(bbplot)
library(readr)

# Import Data -------------------------------------------------------------

ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")    

# Graphs ------------------------------------------------------------------

gg_ramen_ratings <- ramen_ratings %>% 
  na.omit(ramen_ratings$stars) %>% 
  group_by(style) %>%
  dplyr::summarize(mean_stars = mean(stars)) %>% 
  ggplot(aes(
    x = style,
    y = mean_stars,
    fill = style)
    ) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, size = 1, color = "#333333") +
  bbc_style() +
  scale_fill_d3() +
  labs(
    title = "Ramen Ratings",
    subtitle = "Average Ramen Rating by Style, Stars 1-5"
    ) 

finalise_plot(plot_name = gg_ramen_ratings,
              source = "Source: The Ramen Rater",
              save_filepath = "ramen_ratings.PDF",
              width_pixels = 640,
              height_pixels = 550)

gg_ramen_flavor  <- ramen_ratings %>%
    na.omit(ramen_ratings$stars) %>%
    group_by(variety) %>%
    ggplot(aes(x = stars, y = style)) + 
    geom_point(aes(fill = variety, color = variety)) + 
    theme_minimal()


