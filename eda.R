library(dplyr)
library(ggplot2)
library(RColorBrewer)

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
    annotate('rect', xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42,
             color = 'dark red', size = 0.75, alpha = 0) +
    annotate('segment', x = -6, y = 0, xend = 6, yend = 0, size = 1.0, color = 'black') +
    xlim(-6,6) + ylim(-2,6) +
    theme_classic() +
    guides(fill = FALSE) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank())
    ggsave(filename = './plots/hex_pitches.png', width = 150, height = 100, units = 'mm', dpi = 500)

pitches %>%
    filter(pitch_des == 'Called Strike') %>%
    ggplot(data = ., aes(x = px, y = pz)) +
    geom_hex(bins = 40) +
    annotate('rect', xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42,
             color = 'dark red', size = 1.25, alpha = 0) +
    annotate('segment', x = -6, y = 0, xend = 6, yend = 0, size = 1.0, color = 'black') +
    xlim(-6,6) + ylim(-2,6) +
    theme_classic() +
    guides(fill = FALSE) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank())
    ggsave(filename = './plots/hex_strikes.png', width = 150, height = 100, units = 'mm', dpi = 500)

pitches %>%
    filter(pitch_des == 'Ball') %>%
    ggplot(data = ., aes(x = px, y = pz)) +
    geom_hex(bins = 40) +
    annotate('rect', xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42,
             color = 'dark red', size = 1.25, alpha = 0) +
    annotate('segment', x = -6, y = 0, xend = 6, yend = 0, size = 1.0, color = 'black') +
    xlim(-6,6) + ylim(-2,6) +
    theme_classic() +
    guides(fill = FALSE) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank())
    ggsave(filename = './plots/hex_balls.png', width = 150, height = 100, units = 'mm', dpi = 500)



pitches %>%
    filter(pitch_des == 'Called Strike') %>%
    inner_join(y = atbats, by = c('game_pk','batter_id','pitcher_id','atbat_num','event_num')) %>%
    ggplot(data = ., aes(x = px, y = pz, color = batter_stance)) +
    geom_point(alpha = 0.5, size = 1.5) +
    scale_color_brewer(palette = 'Set1') +
    annotate('rect', xmin = -0.83, xmax = 0.83, ymin = 1.52, ymax = 3.42,
             color = 'black', size = 1.25, alpha = 0) +
    xlim(-2,2) + ylim(1,4) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'bottom')
    ggsave(filename = './plots/strikes_by_stance.png', width = 150, height = 100, units = 'mm', dpi = 500)





























