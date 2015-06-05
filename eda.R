library(dplyr)
library(ggplot2)

# Trial Connect
pitches <- tbl_df(read.table(file = './data_parsed/pitches_201408.csv',
                             header = TRUE, sep = '|', quote = ''))
atbats <- tbl_df(read.table(file = './data_parsed/atbats_201408.csv',
                            header = TRUE, sep = '|', quote = ''))


games <- read.table(file = './data_parsed/game_2014.csv',
                      header = TRUE, sep = '|', quote = '')

game_actions <- read.table(file = './data_parsed/game_actions_2014.csv',
                           header = TRUE, sep = '|', quote = '')

game_hits <- read.table(file = './data_parsed/game_hits_2014.csv',
                        header = TRUE, sep = '|', quote = '')

game_players <- read.table(file = './data_parsed/game_players_2014.csv',
                           header = TRUE, sep = '|', quote = '')

game_runners <- read.table(file = './data_parsed/game_runners_2014.csv',
                           header = TRUE, sep = '|', quote = '')

game_teams <- read.table(file = './data_parsed/game_teams_2014.csv',
                         header = TRUE, sep = '|', quote = '')

game_umpires <- read.table(file = './data_parsed/game_umpires_2014.csv',
                           header = TRUE, sep = '|', quote = '')








unique(pitches$pitch_des)

# EDA Visuals
ggplot(data = pitches, aes(x = px, y = pz)) +
    geom_point(alpha = 0.05)




ggplot(data = pitches, aes(x = px, y = pz)) +
    geom_hex(bins = 40) +
    xlim(-8,8) + ylim(-3,8) +
    ggsave(filename = './plots/hex_pitches.png', dpi = 500)

pitches %>%
    filter(pitch_des == 'Called Strike') %>%
    ggplot(data = ., aes(x = px, y = pz)) +
    geom_hex(bins = 40) +
    xlim(-8,8) + ylim(-3,8) +
    ggsave(filename = './plots/hex_strikes.png', dpi = 500)

pitches %>%
    filter(pitch_des == 'Ball') %>%
    ggplot(data = ., aes(x = px, y = pz)) +
    geom_hex(bins = 40) +
    xlim(-8,8) + ylim(-3,8) +
    ggsave(filename = './plots/hex_balls.png', dpi = 500)


# TODO: inner join key error
pitches %>%
    filter(pitch_des == 'Called Strike') %>%
    inner_join(y = atbats, by = 'batter_id') %>%
    filter(batter_stance == 'R') %>%
    ggplot(data = ., aes(x = px, y = pz)) +
    geom_hex(bins = 40) +
    xlim(-2,2) + ylim(1,4)


pitches %>%
    filter(pitch_des == 'Called Strike') %>%
    inner_join(y = atbats, by = c('game_pk','batter_id')) %>%
    ggplot(data = ., aes(x = px, y = pz, color = batter_stance)) +
    geom_point(alpha = 0.5) +
    xlim(-2,2) + ylim(1,4) +
    ggsave(filename = './plots/strikes_by_stance.png', dpi = 500)












