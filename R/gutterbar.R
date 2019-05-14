library(dplyr)
library(ggplot2)
library(lubridate)
library(httr)
library(patchwork)

con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                      dbname = 'spotty',
                      host = 'spotty.cgszne64zlop.us-west-2.rds.amazonaws.com',
                      user = 'alex',
                      port = 5432
)

con <- odbc::dbConnect(odbc::odbc(),  "Spotty")
plays_tbl <- tbl(con, "historyalex")
track_metadata <- readRDS("~/trackmetadata.RDS")

con <- odbc::dbConnect(odbc::odbc(),  "Spotty")



plays_bare <- plays_tbl %>%
  group_by(track_id, track_name, duration_ms, played_at) %>%
  summarise(dup_count = n()) %>%
  collect()

plays <- plays_bare %>%
  left_join(track_metadata) %>%
  ungroup()
###
start_date <- lubridate::as_datetime('2018-07-23')

start_date <- lubridate::as_datetime('2018-08-23')

end_date <- start_date + lubridate::days(7)

scope <- plays %>%
  filter(played_at >= start_date & played_at < end_date)

top_artists <- scope %>%
  group_by(track_artist1_id, track_artist1) %>%
  summarise(cnt = n(),
            distinct_songs = n_distinct(track_id),
            duration_min = sum(duration_ms) / 1000 / 60) %>%
  arrange(desc(cnt))

top_songs <- scope %>%
  group_by(track_artist1_id, track_artist1, track_id, track_name) %>%
  summarise(cnt = n(),
            duration_min = sum(duration_ms) / 1000 / 60) %>%
  arrange(desc(cnt))


# input:
# a set of plays
# a sessionization function (for now just)

# takes a df, a timestamp field, a duration
# returns a df with new field session with an index
sessionize <-
  function(.data, ts_var, duration) {
    ts_var_quo <- enquo(ts_var)

    .data %>%
      arrange(!!ts_var_quo) %>%
      mutate(session = !!ts_var_quo - lag(!!ts_var_quo, default = TRUE) > duration,
             session = cumsum(session))
  }


sessions <- sessionize(scope, played_at, lubridate::minutes(10))

bad_version_1 <- sessions %>%
  ggplot(aes(x = played_at, y = duration_ms/1000/60)) +
  geom_point(shape = 3) +
  facet_grid(day ~ session, scales = "free", space = "free_x")

sessions %>%
  mutate(day = lubridate::wday(played_at)) %>%
  group_by(day) %>%
  mutate(rel_played_at = played_at - first(played_at),
         gap = played_at - lag(played_at),
         gap = coalesce(gap, 0),
         gap2 = if_else(session != lag(session), gap, 0),
         gap2 = coalesce(gap2, 0),
         gap2 = as.numeric(gap2, units = "secs"),
         cumgap = as.difftime(cumsum(gap2), units = "secs")) %>%
  ungroup %>%
  ggplot(aes(x = rel_played_at - cumgap, y = session)) +
  geom_point() +
  facet_wrap(~day, ncol = 1, scales = "free_y")


offset <- 60*5
y_adj <- .3

sessioned <- sessions %>%
  mutate(day = lubridate::wday(played_at)) %>%
  group_by(day) %>%
  mutate(rel_played_at = played_at - first(played_at),
         gap = played_at - lag(played_at),
         gap = coalesce(gap, 0),
         gap2 = if_else(session != lag(session), gap - offset, 0),
         gap2 = coalesce(gap2, 0),
         gap2 = as.numeric(gap2, units = "secs"),
         cumgap = as.difftime(cumsum(gap2), units = "secs")) %>%
  ungroup %>%
  ggplot(aes(x = rel_played_at - cumgap, y = day)) +
  geom_point()


sessioned <- sessions %>%
  mutate(day = lubridate::wday(played_at)) %>%
  group_by(day) %>%
  mutate(rel_played_at = played_at - first(played_at),
         gap = played_at - lag(played_at),
         gap = coalesce(gap, 0),
         gap2 = if_else(session != lag(session), gap - offset, 0),
         gap2 = coalesce(gap2, 0),
         gap2 = as.numeric(gap2, units = "secs"),
         cumgap = as.difftime(cumsum(gap2), units = "secs")) %>%
  mutate(same_artist_sign = track_artist1_id == lag(track_artist1_id, default = 'XXX'),
         same_artist_pos = 2 * (same_artist_sign - .5),
         cumprod_artist = cumprod(same_artist_pos) * y_adj) %>%
  ungroup %>%
  mutate(x = rel_played_at - cumgap)

agg <- sessioned %>%
  group_by(day, session) %>%
  summarise(session_start = min(x),
         session_end = max(x))

p_good <- ggplot() +
  geom_rect(data = agg, aes(xmin = session_start,
                            xmax = session_end,
                            ymin = day - y_adj,
                            ymax = day + .3),
            fill = "#EEF793") +
  geom_point(data = sessioned,
            aes(x = x, y = day + .5, group = session),
            color = "#64BFEA",
            shape = 124) +
  theme_minimal()

jagged <-
ggplot() +
 geom_rect(data = agg, aes(xmin = session_start,
                            xmax = session_end,
                            ymin = day - y_adj,
                            ymax = day + y_adj),
            fill = "#EEF793") +
  geom_point(data = sessioned,
             aes(x = x, y = day + cumprod_artist),
             color = "#64BFEA",
             shape = 124) +
  geom_line(data = sessioned,
             aes(x = x, y = day + cumprod_artist, group = paste0(session,'x',day)),
             color = "#64BFEA", alpha = .3) +
  theme_minimal()

durbar <- sessioned %>%
  group_by(day) %>%
  summarise(dur = sum(duration_ms)) %>%
  ggplot(aes(x = day, y = dur)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

library(patchwork)

durbar / jagged

## what color for session
## what color for points
## what position for points
## adjust session gap to end of last song rather than beginning
## convert UTC to Pacific


## verify that duration is song length and not play length
### VERFIFIED

## Verify monday is 1

# TODO check what happens when I pause play, etc.
# pause resume pause resume
# play 1/5 of the song and skip
# play half the song and skip
# play 4/5 of the song and skip


dows <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
jagged +
  scale_y_continuous(breaks = 1:7, labels = dows)

ggplot() +
  geom_rect(data = agg, aes(xmin = session_start,
                            xmax = session_end,
                            ymin = day - y_adj,
                            ymax = day + y_adj),
            fill = "#EEF793") +
  geom_point(data = sessioned,
             aes(x = x, y = day + cumprod_artist),
             color = "#64BFEA",
             shape = 124) +
  geom_line(data = sessioned,
            aes(x = x, y = day + cumprod_artist, group = paste0(session,'x',day)),
            color = "#64BFEA", alpha = .3) +
  theme_minimal()
