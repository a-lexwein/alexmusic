f <- function(i, tm) {
  x <- tm[[i]]
  list(playlist_track_number = i,
       track_name = x$name,
       track_id = x$id,
       track_artist1 = x$artists[[1]]$name,
       track_artist1_id = x$artists[[1]]$id,
       # track_artists_mess = x$artists,
       track_album = x$album$name,
       track_album_id = x$album$id)
}
tm <- get_tracks(uri, HeaderValue)

1:50 %>%
  purrr::map_df(f, tm = tm) %>%
  View


kweli <- f(7)
abc <- f(8)

----

  tracks_concat <- tracks %>%
  head(50) %>%
  paste(collapse = ",")


uri <- paste0("https://api.spotify.com/v1/tracks/?ids=", tracks_concat)
HeaderValue = paste0("Bearer ", token)

track_metadata <- GET(uri, add_headers(Authorization = HeaderValue)) %>%
  content
track_metadata <- track_metadata$tracks


g <- function(i) {paste0(tracks[i:(i + 50)], collapse = ",")}

g(2, tracks)



1:50 %>%
  purrr::map_df(f)



x <- out %>%
  purrr::map(f)

gt <- tracks_to_uris(tracks)[1] %>%
  get_tracks(HeaderValue)

f(2,gt)


metadata_to_df <- function(tracks_metadata) {
  1:50 %>%
    purrr::map_df(f, tracks_metadata)
}

metadata_to_df(gt)


tracks_to_uris(tracks) %>%
  .[1:2] %>%


  uris <- tracks_to_uris(tracks)
num_uris <- length(uris)
counter <- 1
step <- 25
out <- c()
while (counter <= num_uris) {
  out_piece <- uris %>%
    .[counter:counter+step] %>%
    purrr::map(get_tracks, HeaderValue)

  out <- c(out, out_piece)
  counter <- counter + step
}




###


colnames(plays)
colnames(all_track_metadata)


data <- plays %>%
  ungroup %>%
  select(-track_name) %>%
  left_join(all_track_metadata)


data %>%
  group_by(track_artist1) %>%
  summarise(playcount = n(), hrs = sum(duration_ms)/(60*60*1000)) %>%
  arrange(desc(playcount)) %>%
  head(20)

data %>%
  group_by(track_artist1) %>%
  summarise(playcount = n()) %>%
  arrange(desc(playcount)) %>% View
head(20)

min(data$played_at)


library(lubridate)

artists <- c("Robyn", "Tove Styrke", "Mitski", "Bee Gees")
data %>%
  #filter(track_artist1 %in% artists) %>%
  mutate(week = floor_date(played_at, "week"),
         duration_hr = duration_ms/(60*60*1000)) %>%
  group_by(week) %>%
  summarise(hours = sum(duration_hr)) %>% View
ggplot(aes(x = week, xend = week, y = hours, yend = 0)) +
  geom_segment()


artists <- c("Robyn", "Tove Styrke", "Mitski", "Bee Gees")
data %>%
  #filter(track_artist1 %in% artists) %>%
  mutate(week = floor_date(played_at, "week"),
         duration_hr = duration_ms/(60*60*1000)) %>%
  group_by(week, track_artist1) %>%
  summarise(hours = sum(duration_hr)) %>%
  ggplot(aes(x = week,  y = hours, color = track_artist1)) +
  geom_point()


data %>%
  #filter(track_artist1 %in% artists) %>%
  mutate(week = floor_date(played_at, "week"),
         duration_hr = duration_ms/(60*60*1000)) %>%
  group_by(week, track_artist1) %>%
  summarise(hours = sum(duration_hr), playcount = n()) %>%
  arrange(desc(playcount)) %>%
  group_by(week) %>%
  filter(row_number() <= 10) %>%
  View

data %>%
  ungroup %>%
  filter(played_at <= "2018-10-07" & played_at >= "2018-09-23") %>%
  mutate(week = floor_date(played_at, "day"),
         duration_hr = duration_ms/(60*60*1000)) %>%
  group_by(week) %>%
  summarise(hours = sum(duration_hr)) %>%
  ggplot(aes(x = week, xend = week, y = hours, yend = 0)) +
  geom_segment()

library(patchwork)

artists <- c("Robyn", "Tove Styrke", "Mitski", "Bee Gees", "Lucy Dacus")
bees <- data %>%
  filter(track_artist1 %in% artists) %>%
  ggplot(aes(x = played_at, y = track_artist1, color = track_artist1)) +
  ggbeeswarm::geom_quasirandom(groupOnX = FALSE, size = 1, alpha = .6) +
  theme(legend.position = "none") +
  ggtitle("Alex's Most listened to Artists")

steps <- data %>%
  filter(track_artist1 %in% artists) %>%
  group_by(track_artist1) %>%
  arrange(played_at) %>%
  mutate(playcount = row_number()) %>%
  ggplot(aes(x = played_at, y = playcount, color = track_artist1)) +
  geom_step() +
  theme(legend.position = "top")


bees / steps
