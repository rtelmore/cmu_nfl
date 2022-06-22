## Ryan Elmore
## Look at NFL TOs in 2021 season
## 20 June 2022
 
library(dplyr)
library(nflreadr)
library(magrittr)
library(ggplot2)

df <- nflreadr::load_pbp(season = 2021)

fg_index <- which(df$play_type == "field_goal")

to_before_fg <- df[fg_index - 1, ] %>% 
  dplyr::filter(., timeout == 1, game_id == "2021_05_LA_SEA") 

late_fgs <- df %>% 
  dplyr::filter(., play_id %in% (to_before_fg$play_id + 1),
                game_id %in% to_before_fg$game_id,
                play_type == "field_goal", qtr == 4, 
                half_seconds_remaining <= 180)

la_sea <- df %>% 
  dplyr::filter(., game_id == "2021_05_LA_SEA")

df_sub <- df[fg_index - 1, ] %>% 
  dplyr::mutate(., score_diff = total_home_score - total_away_score)
df_sub %>% 
  dplyr::group_by(., timeout) %>% 
  dplyr::summarize(., n = n())

# timeout     n
# <dbl> <int>
#   1       0   904
# 2       1   158
# 3      NA    14

## How about with less than five minutes left in the game? or half? 
df_sub %>% 
  dplyr::filter(., qtr == 4, half_seconds_remaining < 120) %>% 
  dplyr::group_by(., timeout) %>% 
  dplyr::summarize(., n = n()) %>% 
  dplyr::mutate(freq = n / sum(n))

results <- data.frame(half_seconds = rep(c(30, 60, 90, 120, 150, 180), 2),
                      qtr = rep(c(2, 4), each = 6),
                      timeouts = rep(NA, 12),
                      proportion = rep(NA, 12))
for(i in seq_along(results$qtr)){
#  i <- 4
  tmp <- df_sub %>% 
    dplyr::filter(., qtr == results$qtr[i], half_seconds_remaining < results$half_seconds[i]) %>% 
    dplyr::group_by(., timeout) %>% 
    dplyr::summarize(., n = n()) %>% 
    dplyr::mutate(freq = n / sum(n))
  results[i, 3:4] <- c(tmp$n[2], tmp$freq[2])
}

p <- ggplot(data = results,
            aes(x = half_seconds, y = timeouts, group = qtr, 
                color = as.factor(qtr)))
p + geom_point() +
  geom_line(lwd = 1.2) +
  theme_bw() +
  scale_color_brewer("quarter", palette = "Set1") +
  labs(x = "seconds remaining in the half",
       y = "timeouts called before FG") +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits = c(0, 70)) +
  lims(x = c(0, 200))
ggsave("fig/timeout.png", height = 6, width = 8)

p <- ggplot(data = results,
            aes(x = half_seconds, y = proportion, group = qtr, 
                color = as.factor(qtr)))
p + geom_point() +
  geom_line(lwd = 1.2) +
  theme_bw() +
  scale_color_brewer("quarter", palette = "Set1") +
  labs(x = "seconds remaining in the half",
       y = "proportion of FGs with TOs prior") +
  lims(x = c(0, 200))
ggsave("fig/timeout_props.png", height = 6, width = 8)


# # A tibble: 3 × 2
# timeout     n
# <dbl> <int>
#   1       0    76
# 2       1    62
# 3      NA     6

tt <- df_sub %>% 
  dplyr::group_by(., qtr, timeout, score_diff) %>% 
  dplyr::summarize(., n = n())

to_before_fg <- df[fg_index - 1, ] %>% 
  dplyr::filter(., timeout == 1) 

## score

p <- ggplot(data = df_sub,
            aes(x = score_diff))
p + geom_density()

p <- ggplot(data = df_sub %>% dplyr::filter(!is.na(timeout)),
            aes(x = score_diff, fill = as.factor(timeout)))
p + geom_density(alpha = .5) +
  labs(x = "home team score - away team score") +
  scale_fill_brewer("Timeout", palette = "Set1") +
  theme_bw()
ggsave("fig/tos_score.png", height = 6, width = 8)

p <- ggplot(data = df_sub %>%
              dplyr::filter(., !is.na(timeout), !(qtr %in% c(1, 3, 5))),
            aes(x = score_diff, fill = as.factor(timeout)))

p + geom_density(alpha = .5) +
  labs(x = "home team score - away team score") +
  facet_wrap(~ qtr, ncol = 3, scales = "free") +
  scale_fill_brewer("Timeout", palette = "Set1") +
  theme_bw()
ggsave("fig/tos_score_quarter.png", height = 6, width = 10)

## time remaining

## TOs remaining

## Other variables