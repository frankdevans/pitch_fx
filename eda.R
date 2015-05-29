library(rvest)
library(dplyr)

# TODO: parse runner events within atbats (leverage pitches parser)





















# Parse game (rvest - boxscore)
parse_game <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw <- xml(x = dir_boxscore)
    root <- xml_node(x = raw, xpath = '//boxscore')
    game_df <- data.frame(game_pk = xml_attr(x = root, name = 'game_pk'),
                          game_id = xml_attr(x = root, name = 'game_id'),
                          stadium_id = xml_attr(x = root, name = 'venue_id'),
                          date = xml_attr(x = root, name = 'date'),
                          team_id_home = xml_attr(x = root, name = 'home_id'),
                          team_id_away = xml_attr(x = root, name = 'away_id'),
                          stringsAsFactors = FALSE)
    return(game_df)
}
parse_game(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_teams (boxscore)
parse_game_teams <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw <- xml(x = dir_boxscore)
    root <- xml_node(x = raw, xpath = '//boxscore')
    home_df <- data.frame(game_pk = xml_attr(x = root, name = 'game_pk'),
                          role = 'home',
                          team_id = xml_attr(x = root, name = 'home_id'),
                          wins = xml_attr(x = root, name = 'home_wins'),
                          losses = xml_attr(x = root, name = 'home_loss'),
                          stringsAsFactors = FALSE)
    away_df <- data.frame(game_pk = xml_attr(x = root, name = 'game_pk'),
                          role = 'away',
                          team_id = xml_attr(x = root, name = 'away_id'),
                          wins = xml_attr(x = root, name = 'away_wins'),
                          losses = xml_attr(x = root, name = 'away_loss'),
                          stringsAsFactors = FALSE)
    game_teams_df <- rbind_all(list(home_df, away_df))
    return(game_teams_df)
}
parse_game_teams(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_umpires
parse_game_umpires <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw_boxscore <- xml(x = dir_boxscore)
    root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
    
    dir_players <- paste(dir_base, '/players.xml', sep = '')
    raw_players <- xml(x = dir_players)
    root_umpires <- xml_node(x = raw_players, xpath = '//game/umpires')
    
    l_umpires <- xml_children(root_umpires)
    gu_df <- data.frame(game_pk = xml_attr(x = root_boxscore, name = 'game_pk'),
                        umpire_id = as.character(lapply(X = l_umpires, FUN = xml_attr, name = 'id')),
                        position = as.character(lapply(X = l_umpires, FUN = xml_attr, name = 'position')),
                        name_first = as.character(lapply(X = l_umpires, FUN = xml_attr, name = 'first')),
                        name_last = as.character(lapply(X = l_umpires, FUN = xml_attr, name = 'last')),
                        stringsAsFactors = FALSE)
    return(gu_df)
}
parse_game_umpires(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_coaches
parse_game_coaches <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw_boxscore <- xml(x = dir_boxscore)
    root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
    
    dir_players <- paste(dir_base, '/players.xml', sep = '')
    raw_players <- xml(x = dir_players)
    root_game <- xml_node(x = raw_players, xpath = '//game')
    
    l_game <- xml_children(root_game)
    l_game <- l_game[names(l_game) == 'team']
    names(l_game) <- c('team1','team2')
    
    # Team 1
    root_t1 <- xml_node(x = l_game[['team1']], xpath = '.')
    l_coaches_t1 <- xml_children(root_t1)
    l_coaches_t1 <- l_coaches_t1[names(l_coaches_t1) == 'coach']
    
    df_t1 <- data.frame(game_pk = xml_attr(x = root_boxscore, name = 'game_pk'),
                        team_type = xml_attr(x = root_t1, name = 'type'),
                        coach_id = as.character(lapply(X = l_coaches_t1, FUN = xml_attr, name = 'id')),
                        position = as.character(lapply(X = l_coaches_t1, FUN = xml_attr, name = 'position')),
                        name_first = as.character(lapply(X = l_coaches_t1, FUN = xml_attr, name = 'first')),
                        name_last = as.character(lapply(X = l_coaches_t1, FUN = xml_attr, name = 'last')),
                        number = as.character(lapply(X = l_coaches_t1, FUN = xml_attr, name = 'num')),
                        stringsAsFactors = FALSE)
    
    # Team 2
    root_t2 <- xml_node(x = l_game[['team2']], xpath = '.')
    l_coaches_t2 <- xml_children(root_t2)
    l_coaches_t2 <- l_coaches_t2[names(l_coaches_t2) == 'coach']
    
    df_t2 <- data.frame(game_pk = xml_attr(x = root_boxscore, name = 'game_pk'),
                        team_type = xml_attr(x = root_t2, name = 'type'),
                        coach_id = as.character(lapply(X = l_coaches_t2, FUN = xml_attr, name = 'id')),
                        position = as.character(lapply(X = l_coaches_t2, FUN = xml_attr, name = 'position')),
                        name_first = as.character(lapply(X = l_coaches_t2, FUN = xml_attr, name = 'first')),
                        name_last = as.character(lapply(X = l_coaches_t2, FUN = xml_attr, name = 'last')),
                        number = as.character(lapply(X = l_coaches_t2, FUN = xml_attr, name = 'num')),
                        stringsAsFactors = FALSE)
    
    df_coaches <- rbind_all(list(df_t1, df_t2))
    return(df_coaches)
}
parse_game_coaches(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_players
parse_game_players <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw_boxscore <- xml(x = dir_boxscore)
    root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
    
    dir_players <- paste(dir_base, '/players.xml', sep = '')
    raw_players <- xml(x = dir_players)
    root_game <- xml_node(x = raw_players, xpath = '//game')
    
    l_game <- xml_children(root_game)
    l_game <- l_game[names(l_game) == 'team']
    names(l_game) <- c('team1','team2')
    
    # Team 1
    root_t1 <- xml_node(x = l_game[['team1']], xpath = '.')
    l_players_t1 <- xml_children(root_t1)
    l_players_t1 <- l_players_t1[names(l_players_t1) == 'player']
    
    df_t1 <- data.frame(game_pk = xml_attr(x = root_boxscore, name = 'game_pk'),
                        team_id = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'team_id')),
                        player_id = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'id')),
                        name_first = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'first')),
                        name_last = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'last')),
                        number = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'num')),
                        position = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'position')),
                        game_position = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'game_position')),
                        rl = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'rl')),
                        bat_order = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'bat_order')),
                        bats = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'bats')),
                        status = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'status')),
                        batting_avg = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'avg')),
                        homeruns = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'hr')),
                        rbi = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'rbi')),
                        p_wins = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'wins')),
                        p_losses = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'losses')),
                        p_era = as.character(lapply(X = l_players_t1, FUN = xml_attr, name = 'era')),
                        stringsAsFactors = FALSE)
    
    # Team 2
    root_t2 <- xml_node(x = l_game[['team2']], xpath = '.')
    l_players_t2 <- xml_children(root_t2)
    l_players_t2 <- l_players_t2[names(l_players_t2) == 'player']
    
    df_t2 <- data.frame(game_pk = xml_attr(x = root_boxscore, name = 'game_pk'),
                        team_id = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'team_id')),
                        player_id = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'id')),
                        name_first = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'first')),
                        name_last = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'last')),
                        number = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'num')),
                        position = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'position')),
                        game_position = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'game_position')),
                        rl = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'rl')),
                        bat_order = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'bat_order')),
                        bats = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'bats')),
                        status = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'status')),
                        batting_avg = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'avg')),
                        homeruns = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'hr')),
                        rbi = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'rbi')),
                        p_wins = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'wins')),
                        p_losses = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'losses')),
                        p_era = as.character(lapply(X = l_players_t2, FUN = xml_attr, name = 'era')),
                        stringsAsFactors = FALSE)
    
    df_players <- rbind_all(list(df_t1, df_t2))
    return(df_players)
}
parse_game_players(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse atbats
parse_atbats <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw_boxscore <- xml(x = dir_boxscore)
    root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
    game_pk <- xml_attr(x = root_boxscore, name = 'game_pk')
    
    dir_inning <- paste(dir_base, '/inning/inning_all.xml', sep = '')
    raw_inning <- xml(x = dir_inning)
    root_game <- xml_node(x = raw_inning, xpath = '//game')
    
    l_innings <- xml_children(root_game)
    atbat_df <- data.frame()
    
    for (inning in l_innings) {
        i_num <- xml_attr(x = inning, name = 'num')
        l_inning_part <- xml_children(inning)
        for (inning_part in l_inning_part) {
            i_part <- xml_tag(inning_part)
            l_atbats <- xml_children(inning_part)
            l_atbats <- l_atbats[names(l_atbats) == 'atbat']
            if (length(l_atbats) > 0) {
                new_atbat <- data.frame(game_pk = game_pk,
                                        inning = i_num,
                                        inning_part = i_part,
                                        batter_id = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'batter')),
                                        pitcher_id = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'pitcher')),
                                        start_tfs = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'start_tfs')),
                                        start_tfs_zulu = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'start_tfs_zulu')),
                                        atbat_num = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'num')),
                                        event_num = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'event_num')),
                                        pitcher_throws = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'p_throws')),
                                        batter_height = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'b_height')),
                                        batter_stance = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'stand')),
                                        runs_home = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'home_team_runs')),
                                        runs_away = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'away_team_runs')),
                                        outcome_event = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'event')),
                                        outcome_balls = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'b')),
                                        outcome_strikes = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 's')),
                                        outcome_outs = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'o')),
                                        stringsAsFactors = FALSE)
                atbat_df <- bind_rows(atbat_df, new_atbat)
            }
            
        }
    }
    return(atbat_df)
}
parse_atbats(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_actions
parse_game_actions <- function(dir_base) {
  dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
  raw_boxscore <- xml(x = dir_boxscore)
  root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
  
  dir_inning <- paste(dir_base, '/inning/inning_all.xml', sep = '')
  raw_inning <- xml(x = dir_inning)
  root_game <- xml_node(x = raw_inning, xpath = '//game')
  
  l_innings <- xml_children(root_game)
  
  actions_df <- data.frame()
  
  for (inning in l_innings) {
    i_num <- xml_attr(x = inning, name = 'num')
    l_inning_part <- xml_children(inning)
    for (inning_part in l_inning_part) {
      i_part <- xml_tag(inning_part)
      l_atbats <- xml_children(inning_part)
      l_atbats <- l_atbats[names(l_atbats) == 'action']
      if (length(l_atbats) > 0) {
        new_actions <- data.frame(game_pk = xml_attr(x = root_boxscore, name = 'game_pk'),
                                  inning = i_num,
                                  inning_part = i_part,
                                  event = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'event')),
                                  event_num = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'event_num')),
                                  tfs = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'tfs')),
                                  tfs_zulu = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'tfs_zulu')),
                                  player_id = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'player')),
                                  pitch_num = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'pitch')),
                                  runs_home = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'home_team_runs')),
                                  runs_away = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'away_team_runs')),
                                  occ_balls = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'b')),
                                  occ_strikes = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 's')),
                                  occ_outs = as.character(lapply(X = l_atbats, FUN = xml_attr, name = 'o')),
                                  stringsAsFactors = FALSE)
        actions_df <- bind_rows(actions_df, new_actions)
      }
    }
  }
  return(actions_df)
}
parse_game_actions(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_hits
parse_game_hits <- function(dir_base) {
  dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
  raw_boxscore <- xml(x = dir_boxscore)
  root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
  
  dir_hit <- paste(dir_base, '/inning/inning_hit.xml', sep = '')
  raw_hit <- xml(x = dir_hit)
  root_hit <- xml_node(x = raw_hit, xpath = '//hitchart')
  
  l_hits <- xml_children(root_hit)
  hits_df <- data.frame(game_pk = xml_attr(x = root_boxscore, name = 'game_pk'),
                        inning = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'inning')),
                        description = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'des')),
                        pitcher_id = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'pitcher')),
                        batter_id = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'batter')),
                        team_type = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'team')),
                        hit_type = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'type')),
                        hit_x = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'x')),
                        hit_y = as.character(lapply(X = l_hits, FUN = xml_attr, name = 'y')),
                        stringsAsFactors = FALSE)
  return(hits_df)
}
parse_game_hits(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse pitches
parse_pitches <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw_boxscore <- xml(x = dir_boxscore)
    root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
    game_pk <- xml_attr(x = root_boxscore, name = 'game_pk')
    
    dir_inning <- paste(dir_base, '/inning/inning_all.xml', sep = '')
    raw_inning <- xml(x = dir_inning)
    root_game <- xml_node(x = raw_inning, xpath = '//game')
    
    l_innings <- xml_children(root_game)
    
    pitches_df <- data.frame()
    
    for (inning in l_innings) {
        i_num <- xml_attr(x = inning, name = 'num')
        l_inning_part <- xml_children(inning)
        for (inning_part in l_inning_part) {
            i_part <- xml_tag(inning_part)
            l_atbats <- xml_children(inning_part)
            l_atbats <- l_atbats[names(l_atbats) == 'atbat']
            if (length(l_atbats) > 0) {
                for (atbat in l_atbats) {
                    l_pitches <- xml_children(atbat)
                    l_pitches <- l_pitches[names(l_pitches) == 'pitch']
                    if (length(l_pitches) > 0) {
                        new_pitches <- data.frame(game_pk = game_pk,
                                                  play_guid = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'play_guid')),
                                                  batter_id = xml_attr(x = atbat, name = 'batter'),
                                                  pitcher_id = xml_attr(x = atbat, name = 'pitcher'),
                                                  atbat_num = xml_attr(x = atbat, name = 'num'),
                                                  event_num = xml_attr(x = atbat, name = 'event_num'),
                                                  pitch_des = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'des')),
                                                  pitch_id = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'id')),
                                                  type = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'type')),
                                                  tfs = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'tfs')),
                                                  tfs_zulu = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'tfs_zulu')),
                                                  x = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'x')),
                                                  y = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'y')),
                                                  pitch_event_num = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'event_num')),
                                                  sv_id = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'sv_id')),
                                                  speed_start = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'start_speed')),
                                                  speed_end = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'end_speed')),
                                                  sz_top = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'sz_top')),
                                                  sz_bot = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'sz_bot')),
                                                  pfx_x = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'pfx_x')),
                                                  pfx_z = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'pfx_z')),
                                                  px = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'px')),
                                                  pz = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'pz')),
                                                  x0 = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'x0')),
                                                  y0 = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'y0')),
                                                  z0 = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'z0')),
                                                  vx0 = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'vx0')),
                                                  vy0 = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'vy0')),
                                                  vz0 = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'vz0')),
                                                  ax = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'ax')),
                                                  ay = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'ay')),
                                                  az = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'az')),
                                                  break_y = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'break_y')),
                                                  break_angle = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'break_angle')),
                                                  break_length = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'break_length')),
                                                  pitch_type = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'pitch_type')),
                                                  type_confidence = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'type_confidence')),
                                                  zone = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'zone')),
                                                  nasty = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'nasty')),
                                                  spin_dir = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'spin_dir')),
                                                  spin_rate = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'spin_rate')),
                                                  cc = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'cc')),
                                                  mt = as.character(lapply(X = l_pitches, FUN = xml_attr, name = 'mt')),
                                                  stringsAsFactors = FALSE)
                        pitches_df <- bind_rows(pitches_df, new_pitches)
                    }
                }
            }
        }
    }
    return(pitches_df)
}
parse_pitches(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')







# Parse with rvest tests
raw <- xml(x = './data/boxscore.xml')
xml_tag(raw)
xml_children(raw)
root <- xml_node(x = raw, xpath = '//boxscore')
xml_attrs(root)
xml_tag(root)
xml_attr(x = root, name = 'game_pk')
xml_children(root)
ls <- xml_node(x = raw, xpath = '//boxscore/linescore')
ls_c <- xml_children(ls)
ls_c[[1]]
xml_attr(x = ls_c[[2]], name = 'home')




# XPath Syntax: http://www.w3schools.com/xpath/xpath_syntax.asp







