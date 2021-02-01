#' Tidy Tuesday Mar 17 2020 
#' Scott Cohn
#' Schrute


# Imports and Data -----------

library(tidyverse) 
library(schrute)
library(tidytext)

office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv') 
# # A tibble: 188 x 6
#    season episode title             imdb_rating total_votes air_date  
#     <dbl>   <dbl> <chr>                   <dbl>       <dbl> <date>    
#  1      1       1 Pilot                     7.6        3706 2005-03-24
#  2      1       2 Diversity Day             8.3        3566 2005-03-29
#  3      1       3 Health Care               7.9        2983 2005-04-05
#  4      1       4 The Alliance              8.1        2886 2005-04-12
#  5      1       5 Basketball                8.4        3179 2005-04-19
#  6      1       6 Hot Girl                  7.8        2852 2005-04-26
#  7      2       1 The Dundies               8.7        3213 2005-09-20
#  8      2       2 Sexual Harassment         8.2        2736 2005-09-27
#  9      2       3 Office Olympics           8.4        2742 2005-10-04
# 10      2       4 The Fire                  8.4        2713 2005-10-11
# # … with 178 more rows

schrute_df  <-  schrute::theoffice

# Explore ------------

office_ratings %>% 
    select(title, imdb_rating, total_votes, season) %>%
    filter(imdb_rating > 9) %>%
    ggplot() +
    geom_col(aes(x = title, y = imdb_rating, fill = factor(season))) + 
    coord_flip()

# dplyr::glimpse(schrute_df)

token_schrute <- schrute_df %>%
    tidytext::unnest_tokens(word, text)


