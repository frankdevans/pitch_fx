library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(caret)
library(dplyr)



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

elias_lahman <- read.table(file = './data/elias_to_lahman.csv',
                           header = TRUE,
                           sep = ',',
                           stringsAsFactors = FALSE)

# Derived Data
hit_weight <- data_frame(outcome_bin = c('Hit','Ball','Strike','Foul','Out'),
                         hit_weight = c(1.0, 0.25, 0.0, 0.0, 0.0))

outcome_onbase <- c('Single','Double','Triple','HomeRun')


# Register Functions
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
team_pitchers <- function(team_id_num) {
    pitchers <- pitches %>%
        select(game_pk, pitcher_id) %>%
        inner_join(y = game_players %>%
                       select(game_pk, player_id, team_id),
                   by = c('game_pk','pitcher_id' = 'player_id')) %>%
        filter(team_id == team_id_num) %>%
        group_by(pitcher_id) %>%
        summarise(num_pitches = n(),
                  num_games = length(unique(game_pk))) %>%
        arrange(desc(num_pitches))
    return(pitchers$pitcher_id[1:5])
}
batter_mhw_game <- function(batter_id_num) {
    output <- pitches %>%
        filter(batter_id == batter_id_num) %>%
        select(game_pk, batter_id, pitch_des) %>%
        inner_join(y = bin_pitch, by = 'pitch_des') %>%
        inner_join(y = hit_weight, by = 'outcome_bin') %>%
        select(game_pk, hit_weight, outcome_bin) %>%
        group_by(game_pk) %>%
        summarise(hit_weight = sum(hit_weight)) %>%
        ungroup() %>%
        group_by() %>%
        summarise(mean_hits = mean(hit_weight))
    return(output$mean_hits)
}
pitcher_mhw_game <- function(pitcher_id_num) {
    output <- pitches %>%
        filter(pitcher_id == pitcher_id_num) %>%
        select(game_pk, batter_id, pitch_des) %>%
        inner_join(y = bin_pitch, by = 'pitch_des') %>%
        inner_join(y = hit_weight, by = 'outcome_bin') %>%
        select(game_pk, hit_weight, outcome_bin) %>%
        group_by(game_pk) %>%
        summarise(hit_weight = sum(hit_weight)) %>%
        ungroup() %>%
        group_by() %>%
        summarise(mean_hits = mean(hit_weight))
    return(output$mean_hits)
}
exp_game_outcome <- function(batter_id_num, pitcher_id_num) {
    b_set <- pitches %>%
        filter(batter_id == batter_id_num) %>%
        filter(!is.na(speed_end)) %>%
        inner_join(y = bin_pitch, by = 'pitch_des') %>%
        mutate(outcome = as.factor(outcome_bin)) %>%
        select(outcome, speed_end, px, pz, break_y, 
               break_angle, break_length, spin_rate,vx0, vy0, vz0, z0)
    
    
    rp_model <- train(outcome ~ .,
                      data = b_set,
                      method = 'rpart',
                      trControl = trainControl(method = "cv", 
                                               number = 4, 
                                               allowParallel = TRUE, 
                                               verboseIter = TRUE))
    
    pitches_per_game <- pitches %>%
        filter(batter_id == batter_id_num) %>%
        group_by(game_pk) %>%
        summarise(num_pitches = n()) %>%
        ungroup() %>%
        group_by() %>%
        summarise(pitches_per_game = mean(num_pitches)) %$%
        pitches_per_game
    
    mean_hw <- b_set %>%
        select(outcome) %>%
        inner_join(y = hit_weight, by = c('outcome' = 'outcome_bin')) %>%
        group_by() %>%
        summarise(mean_hw = mean(hit_weight)) %$%
        mean_hw
    
    
    p_set <- pitches %>%
        filter(pitcher_id == pitcher_id_num) %>%
        filter(!is.na(speed_end)) %>%
        select(pitch_type, speed_end, px, pz, break_y, 
               break_angle, break_length, spin_rate,vx0, vy0, vz0, z0) %>%
        mutate(prediction = as.character(predict(rp_model, .))) %>%
        inner_join(y = hit_weight, by = c('prediction' = 'outcome_bin')) %>%
        group_by(pitch_type) %>%
        summarise(exp_hw = mean(hit_weight),
                  pitches = n()) %>%
        ungroup() %>%
        group_by() %>%
        mutate(total_pitches = sum(pitches)) %>%
        ungroup() %>%
        mutate(pitch_pct = (pitches / total_pitches)) %>%
        filter(pitch_pct > 0.01) %>%
        select(-c(pitches, total_pitches)) %>%
        mutate(oa_total_hw = batter_mhw_game(batter_id_num = batter_id_num),
               pitches_per_game = pitches_per_game,
               sp_total_hw = (exp_hw * pitches_per_game)) %>%
        mutate(pitcher_id = pitcher_id_num,
               batter_id = batter_id_num) %>%
        select(pitcher_id, batter_id, 1:7) %>%
        mutate(net_change = (sp_total_hw - oa_total_hw))
    return(p_set)
}
pid_to_fullname <- function(pid) {
    output <- game_players %>%
        tbl_df() %>%
        filter(player_id == pid) %>%
        mutate(name = paste(name_first, name_last, sep = ' ')) %>%
        select(name) %>%
        group_by(name) %>%
        summarise(recs = n()) %>%
        arrange(desc(recs)) %$%
        name[1]
    return(output)
}



# Build Analysis
name_pitch_team <- 'Rangers'
name_bat_team <- 'Angels'


bench_pitchers <- dim_teams %>%
    tbl_df(data = .) %>%
    select(team_id, location_name, team_name) %>%
    filter(team_name == name_pitch_team) %$%
    team_pitchers(team_id_num = team_id)

pit_batters <- dim_teams %>%
    tbl_df(data = .) %>%
    select(team_id, location_name, team_name) %>%
    filter(team_name == name_bat_team) %$%
    team_lineup(team_id_num = team_id)



hits_lm <- pitches %>%
    inner_join(y = bin_pitch, by = 'pitch_des') %>%
    inner_join(y = hit_weight, by = 'outcome_bin') %>%
    select(game_pk, hit_weight) %>%
    group_by(game_pk) %>%
    summarise(hit_weight = sum(hit_weight)) %>%
    inner_join(y = atbats %>%
                   select(game_pk, outcome_event) %>%
                   mutate(outcome_hit = (outcome_event %in% outcome_onbase)) %>%
                   group_by(game_pk) %>%
                   summarise(hits = abs(sum(outcome_hit) - 6)),
               by = 'game_pk') %>%
    lm(formula = (hits ~ hit_weight), data = .)



pitch_sim <- data_frame()
for (p in bench_pitchers) {
    for (b in pit_batters) {
        pitch_sim <- bind_rows(pitch_sim,
                               exp_game_outcome(batter_id_num = b,
                                                pitcher_id_num = p))
    }
}


pitch_sim %<>%
    mutate(oa_total_hits = (hits_lm$coefficients[1] + (oa_total_hw * hits_lm$coefficients[2])),
           sp_total_hits = (hits_lm$coefficients[1] + (sp_total_hw * hits_lm$coefficients[2])),
           net_change_hits = (net_change * hits_lm$coefficients[2])) %>%
    arrange(net_change_hits) %>%
    mutate(pitcher_name = sapply(X = pitcher_id, FUN = pid_to_fullname),
           batter_name = sapply(X = batter_id, FUN = pid_to_fullname),
           text_disp = paste(pitcher_name, pitch_type, batter_name, sep = ' '))

ggplot(data = pitch_sim) +
    geom_histogram(mapping = aes(net_change_hits), fill = 'darkred') +
    labs(title = paste(name_pitch_team, 'pitching to', name_bat_team, sep = ' '),
         x = 'Net Change in Hits Expected per Batter',
         y = '')
ggsave(filename = './plots/net_change_hits.png', width = 150, height = 100, units = 'mm', dpi = 500)



pitch_sim %>%
    select(text_disp, net_change_hits)
    






























# Unit Tests
exp_ab_outcome(batter_id_num = 572761, pitcher_id_num = 458681)
pitcher_mhw_game(458681)
batter_mhw_game(572761)
team_pitchers(138)
team_pitchers(133)
team_pitchers(101)
team_lineup(138)
team_lineup(133)
team_lineup(101)
pid_to_fullname(pid = 285078)
pid_to_fullname(pid = 430947)






# TODO: map from player friendly names to ID (app_preprocess)































