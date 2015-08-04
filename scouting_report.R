library(dplyr)
library(ggplot2)
library(RColorBrewer)



# Load Data
pitches <- tbl_df(read.table(file = './data_parsed/pitches_2014.csv',
                             header = TRUE, sep = '|', quote = ''))

atbats <- tbl_df(read.table(file = './data_parsed/atbats_2014.csv',
                            header = TRUE, sep = '|', quote = ''))

game_players <- read.table(file = './data_parsed/game_players_2014.csv',
                           header = TRUE, sep = '|', quote = '')

bin_pitch <- read.table(file = './data/bin_pitch_des.csv',
                        header = TRUE,
                        sep = '|',
                        stringsAsFactors = FALSE)

dim_teams <- read.table(file = './data/dim_teams.csv',
                        header = TRUE,
                        sep = ',',
                        stringsAsFactors = FALSE)



# TODO: Batting: classify batters by weaknesses in Off-Base results, seek to reduce hitting capability
# as much as possible. Quote outcomes in reduction of decimal anticipated hits.
pitches %>%
    filter(batter_id == 572761) %>%
    select(game_pk, batter_id, pitch_des, speed_end) %>%
    filter(!is.na(speed_end)) %>%
    inner_join(y = bin_pitch, by = 'pitch_des') %>%
    mutate(speed_bin = round(x = speed_end, digits = 0),
           off_base_id = (outcome_bin == 'Hit')) %>%
    group_by(speed_bin) %>%
    summarise(off_base_pct = mean(off_base_id),
              pitces = n()) %>%
    ggplot(data = .) +
    geom_line(mapping = aes(x = speed_bin, y = off_base_pct))







# TODO: Pitching: classify pitchers by strengths in pitching and types of pitches capable






# TODO: Team: use batting function to expand to whole team of batters to quantify reduction in hits expected
team_lineup <- function(team_id_num) {
    batters <- atbats %>%
        select(game_pk, batter_id) %>%
        inner_join(y = game_players %>%
                       select(game_pk, player_id, team_id), 
                   by = c('game_pk','batter_id' = 'player_id')) %>%
        filter(team_id == team_id_num) %>%
        group_by(batter_id) %>%
        summarize(num_atbats = n(),
                  game_appearences = length(unique(game_pk))) %>%
        ungroup() %>%
        arrange(desc(num_atbats))
    return(batters$batter_id[1:9])
}

team_lineup(138)
team_lineup(133)
team_lineup(101)













# TODO: map from player friendly names to ID (app_preprocess)































