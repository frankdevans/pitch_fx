library(dplyr)
library(ggplot2)
library(RColorBrewer)


# Load Data
pitches <- tbl_df(read.table(file = './data_parsed/pitches_2014.csv',
                             header = TRUE, sep = '|', quote = ''))

atbats <- tbl_df(read.table(file = './data_parsed/atbats_2014.csv',
                            header = TRUE, sep = '|', quote = ''))

games <- read.table(file = './data_parsed/game_2014.csv',
                      header = TRUE, sep = '|', quote = '')

game_actions <- tbl_df(read.table(file = './data_parsed/game_actions_2014.csv',
                                  header = TRUE, sep = '|', quote = ''))

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

bin_pitch <- read.table(file = './data/bin_pitch_des.csv',
                        header = TRUE,
                        sep = '|',
                        stringsAsFactors = FALSE)






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
    xlim(-3.5,3.5) + ylim(1,4) +
    theme_classic() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'bottom')
    ggsave(filename = './plots/strikes_by_stance.png', width = 150, height = 100, units = 'mm', dpi = 500)





# TODO: Pitch Lens recreation

















# Pitch Bins
pitches %>%
    group_by(pitch_des) %>%
    summarize(pitches = n()) %>%
    inner_join(y = bin_pitch, by = 'pitch_des') %>%
    arrange(desc(pitches)) %>%
    ggplot(data = .) +
    geom_bar(mapping = aes(x = pitch_des, y = pitches), stat = 'identity') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = 'Type of Pitch')
    ggsave(filename = './plots/bins_pitches_bw.png', width = 150, height = 100, units = 'mm', dpi = 500)


pitches %>%
    group_by(pitch_des) %>%
    summarize(pitches = n()) %>%
    inner_join(y = bin_pitch, by = 'pitch_des') %>%
    arrange(desc(pitches)) %>%
    ggplot(data = .) +
    geom_bar(mapping = aes(x = pitch_des, y = pitches, fill = outcome_bin), stat = 'identity') +
    scale_fill_brewer(palette = 'Set1') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = 'Type of Pitch')
    ggsave(filename = './plots/bins_pitches_rgb.png', width = 150, height = 100, units = 'mm', dpi = 500)


pitches %>%
    inner_join(y = bin_pitch, by = 'pitch_des') %>%
    group_by(outcome_bin) %>%
    summarize(pitches = n()) %>%
    ggplot(data = .) +
    geom_bar(aes(x = outcome_bin, y = pitches, fill = outcome_bin), stat = 'identity') +
    scale_fill_brewer(palette = 'Set1') +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = 'Outcome of Pitch')
    ggsave(filename = './plots/bins_outcome_nopct.png', width = 150, height = 100, units = 'mm', dpi = 500)


pitches %>%
    inner_join(y = bin_pitch, by = 'pitch_des') %>%
    group_by(outcome_bin) %>%
    summarize(pitches = n()) %>%
    ungroup() %>%
    arrange(desc(pitches)) %>%
    group_by() %>%
    mutate(total = sum(pitches)) %>%
    ungroup() %>%
    mutate(pct = pitches / total,
           pct_friendly = paste(round(x = pct * 100, digits = 1), '%')) %>%
    ggplot(data = .) +
    geom_bar(aes(x = outcome_bin, y = pitches, fill = outcome_bin), stat = 'identity') +
    scale_fill_brewer(palette = 'Set1') +
    geom_text(mapping = aes(x = outcome_bin, y = pitches - 10000, label = pct_friendly), fontface = 'bold')
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank()) +
    labs(x = 'Outcome of Pitch')
    ggsave(filename = './plots/bins_outcome_pct.png', width = 150, height = 100, units = 'mm', dpi = 500)






# Probability of Winning given runs scored
game_logs <- tbl_df(read.table(file = './data/game_logs/GL2014.TXT',
                               header = FALSE,
                               sep = ','))

wl_logs <- game_logs %>%
    select(V4, V7, V10, V11, V23, V51) %>%
    rename(team_visit = V4,
           team_home = V7,
           score_visit = V10,
           score_home = V11,
           hits_visit = V23,
           hits_home = V51) %>%
    mutate(winner_visit = (score_visit > score_home),
           winner_home = !winner_visit)

wl_logs %>%
    select(score_visit, winner_visit) %>%
    rename(score = score_visit,
           win = winner_visit)

wl_logs %>%
    select(score_home, winner_home) %>%
    rename(score = score_home,
           win = winner_home)

bind_rows(wl_logs %>%
              select(score_visit, winner_visit) %>%
              rename(score = score_visit,
                     win = winner_visit),
          wl_logs %>%
              select(score_home, winner_home) %>%
              rename(score = score_home,
                     win = winner_home)
          ) %>%
    group_by(score) %>%
    summarise(games = n(),
              win_prob = mean(win)) %>%
    ungroup() %>%
    ggplot(data = .) +
    geom_bar(mapping = aes(x = score, y = games), stat = 'identity', fill = 'steelblue') +
    geom_line(mapping = aes(x = score, y = win_prob), color = 'steelblue', size = 2) +
    labs(x = 'Runs Scored in Game',
         y = 'Number of Game Occurrences')
    ggsave(filename = './plots/hist_win_runs.png', width = 150, height = 100, units = 'mm', dpi = 500)


bind_rows(wl_logs %>%
              select(score_visit, winner_visit) %>%
              rename(score = score_visit,
                     win = winner_visit),
          wl_logs %>%
              select(score_home, winner_home) %>%
              rename(score = score_home,
                     win = winner_home)
        ) %>%
    group_by(score) %>%
    summarise(games = n(),
              win_prob = mean(win)) %>%
    ungroup() %>%
    ggplot(data = .) +
    geom_line(mapping = aes(x = score, y = win_prob), color = 'steelblue', size = 2) +
    annotate(geom = 'segment', 
             x = 0, y = 0.5, xend = 20, yend = 0.5, 
             color = 'black', size = 1.5, linetype = 'longdash') +
    labs(x = 'Runs Scored in Game',
         y = 'Probability of Winning Game')
    ggsave(filename = './plots/prob_win_runs.png', width = 150, height = 100, units = 'mm', dpi = 500)



bind_rows(wl_logs %>%
              select(hits_visit, winner_visit) %>%
              rename(hits = hits_visit,
                     win = winner_visit),
          wl_logs %>%
              select(hits_home, winner_home) %>%
              rename(hits = hits_home,
                     win = winner_home)
          ) %>%
    group_by(hits) %>%
    summarise(games = n(),
              win_prob = mean(win)) %>%
    ungroup() %>%
    ggplot(data = .) +
    geom_line(mapping = aes(x = hits, y = win_prob), color = 'darkred', size = 2) +
    annotate(geom = 'segment', 
             x = 0, y = 0.5, xend = 25, yend = 0.5, 
             color = 'black', size = 1.5, linetype = 'longdash') +
    labs(x = 'Hits in Game',
         y = 'Probability of Winning Game')
    ggsave(filename = './plots/prob_win_hits.png', width = 150, height = 100, units = 'mm', dpi = 500)


bind_rows(wl_logs %>%
              select(hits_visit, winner_visit) %>%
              rename(hits = hits_visit,
                     win = winner_visit),
          wl_logs %>%
              select(hits_home, winner_home) %>%
              rename(hits = hits_home,
                     win = winner_home)
            ) %>%
    group_by(hits) %>%
    summarise(games = n(),
              win_prob = mean(win)) %>%
    ungroup() %>%
    ggplot(data = .) +
    geom_bar(mapping = aes(x = hits, y = games), stat = 'identity', fill = 'darkred') +
    labs(x = 'Hits in Game',
         y = 'Number of Game Occurrences')
    ggsave(filename = './plots/hist_win_hits.png', width = 150, height = 100, units = 'mm', dpi = 500)



# TODO: season winning record levels at team granularity, best to worst
bind_rows(wl_logs %>%
              select(team_visit, winner_visit) %>%
              rename(team = team_visit,
                     win = winner_visit),
          wl_logs %>%
              select(team_home, winner_home) %>%
              rename(team = team_home,
                     win = winner_home)
          ) %>%
    group_by(team) %>%
    summarize(win_rate = mean(win)) %>%
    ungroup() %>%
    arrange(desc(win_rate)) %>%
    mutate(order_index = as.numeric(rownames(.))) %>%
    ggplot(data = .) +
    geom_area(mapping = aes(x = order_index, y = win_rate), stat = 'identity', fill = 'darkblue') +
    annotate(geom = 'segment', 
             x = 1, y = 0.5, xend = 30, yend = 0.5, 
             color = 'black', size = 1.5, linetype = 'longdash') +
    scale_x_continuous(limits = c(1,30)) +
    scale_y_continuous(limits = c(0,1)) +
    theme_classic() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = 'Win Record')
    ggsave(filename = './plots/win_records.png', width = 150, height = 100, units = 'mm', dpi = 500)
    


























