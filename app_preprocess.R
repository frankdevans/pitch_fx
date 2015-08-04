library(stringr)
library(dplyr)
library(ggplot2)




# Load
pitches <- tbl_df(read.table(file = './data_parsed/pitches_2014.csv',
                             header = TRUE, sep = '|', quote = ''))

game_players <- tbl_df(read.table(file = './data_parsed/game_players_2014.csv',
                                  header = TRUE, sep = '|', quote = ''))

game_teams <- tbl_df(read.table(file = './data_parsed/game_teams_2014.csv',
                                header = TRUE, sep = '|', quote = ''))


dim_teams <- read.table(file = './data/dim_teams.csv',
                        header = TRUE,
                        sep = ',',
                        stringsAsFactors = FALSE)

bin_pitch <- read.table(file = './data/bin_pitch_des.csv',
                        header = TRUE,
                        sep = '|',
                        stringsAsFactors = FALSE)


# Build Reference Object
pen_batters <- game_players %>%
    filter(player_id %in% c(545361, 405395, 594777, 429664, 572122, 443558)) %>%
    group_by(player_id, name_first, name_last) %>%
    summarize(games = n()) %>%
    ungroup() %>%
    mutate(name_full = paste(name_first, name_last))


pen_pitchers <- game_players %>%
    filter(team_id == 140) %>%
    filter(position == 'P') %>%
    group_by(player_id, name_first, name_last) %>%
    summarize(games = n()) %>%
    ungroup() %>%
    inner_join(y = pitches %>%
                   group_by(pitcher_id) %>%
                   summarize(num_pitches = n()),
               by = c('player_id' = 'pitcher_id')) %>%
    arrange(desc(num_pitches)) %>%
    filter(games > 100) %>%
    filter(num_pitches > 2000) %>%
    mutate(name_full = paste(name_first, name_last))


pitches %>%
  semi_join(y = pen_pitchers, by = c('pitcher_id' = 'player_id')) %>%
  group_by(pitch_des) %>%
  summarize(pitches = n())



# TODO: bin plate crossing bounds, determine other factors
zone_bin_x <- data_frame(round_x = seq(from = -1.2, to = 1.2, by = 0.01))
zone_bin_x$bin_x <- cut(x = zone_bin_x$round_x, breaks = 8)

zone_bin_y <- data_frame(round_y = seq(from = 1.3, to = 4.0, by = 0.01))
zone_bin_y$bin_y <- cut(x = zone_bin_y$round_y, breaks = 8)



annotate('rect', xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42,
         color = 'dark red', size = 0.75, alpha = 0) +




















