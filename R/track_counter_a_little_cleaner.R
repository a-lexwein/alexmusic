
library(dplyr)
library(ggplot2)
library(lubridate)
library(httr)
library(patchwork)

con <- odbc::dbConnect(odbc::odbc(),  "Spotty")
plays_tbl <- tbl(con, "historyalex")
track_metadata <- readRDS("~/trackmetadata.RDS")

plays_bare <- plays_tbl %>%
  group_by(track_id, track_name, duration_ms, played_at) %>%
  summarise(dup_count = n()) %>%
  collect()

plays <- plays_bare %>%
  left_join(track_metadata) %>%
  ungroup()


artists <- c("Robyn", "Tove Styrke", "Mitski", "Bee Gees", "Lucy Dacus")

plays %>%
  ungroup %>%
  count(track_artist1, sort = TRUE) %>%
  View

artists <- c("Ariana Grande", "King Princess", "Lykke Li", "Adult Mom", "Selena Gomez")

bees <- plays %>%
  filter(track_artist1 %in% artists) %>%
  ggplot(aes(x = played_at, y = track_artist1, color = track_artist1)) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, size = 1, alpha = .6) +
  theme(legend.position = "none") +
  ggtitle("Alex's Most listened to Artists")

steps <- plays %>%
  filter(track_artist1 %in% artists) %>%
  group_by(track_artist1) %>%
  arrange(played_at) %>%
  mutate(playcount = row_number()) %>%
  ggplot(aes(x = played_at, y = playcount, color = track_artist1)) +
  geom_step() +
  theme(legend.position = "top")


bees / steps

week_start <- as.Date("2018-09-02")

week <- plays %>%
  filter(played_at >= week_start & played_at <= (week_start + 7))



plays %>%
  mutate(played_at = with_tz(played_at, "America/Los_Angeles"),
         hr_of_day = (played_at - floor_date(played_at, "day")) / 60 ) %>%
  filter(played_at >= week_start) %>%
  ggplot(aes(x = played_at, y = hr_of_day)) +
  geom_point(size =1, alpha = .3 ) +
  ggtitle("Alex's Spotify Listening Sessions")

count(week, track_artist1, sort = TRUE) %>%
  head(20)
