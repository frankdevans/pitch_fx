library(stringr)
library(dplyr)




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


# TODO: bin pitch_des into play outcomes
unique(pitches$pitch_des)
bin_pitch_des <- data_frame(pitch_des = unique(pitches$pitch_des),
                            pitch_cat = '')




























