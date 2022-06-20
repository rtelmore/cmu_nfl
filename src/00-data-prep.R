## Ryan Elmore
## Look at NFL TOs in 2021 season
## 20 June 2022
 
library(dplyr)
library(nflreadr)
library(magrittr)

df <- nflreadr::load_pbp(season = 2021)

fg_index <- which(df$play_type == "field_goal")

df_sub <- df[fg_index - 1, ]
df_sub %>% 
  dplyr::group_by(., timeout) %>% 
  dplyr::summarize(., n = n())

to_before_fg <- df[fg_index - 1, ] %>% 
  dplyr::filter(., timeout == 1) 

## score

## time remaining

## TOs remaining

## Other variables