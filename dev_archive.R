# Find Files
list.files(path = './data/', recursive = TRUE)
list.files(path = './data/', recursive = TRUE, include.dirs = TRUE)
list.dirs(path = './data', recursive = TRUE)
list.dirs(path = './data', recursive = FALSE, full.names = TRUE)

dirs <- list.dirs(path = './data', recursive = TRUE, full.names = TRUE)
dirs[1]
str_detect(string = dirs[1:5], pattern = 'gid')
str_split(string = dirs[4], pattern = '/')
tail(str_split(string = dirs[4],, pattern = '/')[[1]], n=1)
str_detect(string = tail(str_split(string = dirs[3], pattern = '/')[[1]], n=1), pattern = 'gid')

t1 <- str_split(string = dirs, pattern = '/')
lapply(X = t1, FUN = tail, n = 1)
as.character(lapply(X = t1, FUN = tail, n = 1))

tdf <- data.frame(dirs = dirs,
                  last_dir = as.character(lapply(X = t1, FUN = tail, n = 1)),
                  stringsAsFactors = FALSE)
tdf <- tbl_df(tdf)
tdf <- tdf %>%
    filter(str_detect(string = last_dir, pattern = 'gid') == TRUE) %>%
    select(-(last_dir))
tdf$dirs



# Parse game (XML)
library(XML)
raw <- xmlRoot(xmlParse(file = './data/game.xml'))
children <- xmlChildren(raw)
children
children[[1]]
children[['stadium']]
attrs <- xmlAttrs(children[['stadium']])
attrs[['id']]
data.frame(id = attrs[['id']],
           name = attrs[['name']])

data.frame(id = xmlGetAttr(node = children[['stadium']], name = 'id'),
           name = xmlGetAttr(node = children[['stadium']], name = 'name'),
           game_pk = xmlGetAttr(node = raw, name = 'game_pk'))

data.frame(xmlAttrs(children[['stadium']]))
xmlToDataFrame(children[[1]], colClasses = 'character')


# Helper: parse single atbat (game_pk, i_num, i_part, l_atbats)
parse_single_atbat <- function(game_pk, i_num, i_part, l_atbats) {
    df_atbat <- data.frame(game_pk = game_pk,
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
    return(df_atbat)
}



# Parse game (rvest - boxscore)
parse_game(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_teams (boxscore)
parse_game_teams(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_umpires
parse_game_umpires(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_coaches

parse_game_coaches(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_players
parse_game_players(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse atbats
parse_atbats(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_actions
parse_game_actions(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_hits
parse_game_hits(dir_base = './gid_2014_06_09_atlmlb_colmlb_1')
parse_game_hits(dir_base = './gid_2014_06_09_nyamlb_kcamlb_1')
parse_game_hits(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse pitches
parse_pitches(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Parse game_runners
parse_game_runners(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')

# Check for all necessary files to parse
check_files(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')
check_files(dir_base = './data/day_01/gid_2014_06_01_pitmlb_lanmlb_1')
check_files(dir_base = './data/day_01/gid_2014_06_01_kcamlb_tormlb_1')


library(data.table)
# Parse atbats (data.table)
parse_atbats <- function(dir_base) {
    dir_boxscore <- paste(dir_base, '/boxscore.xml', sep = '')
    raw_boxscore <- xml(x = dir_boxscore)
    root_boxscore <- xml_node(x = raw_boxscore, xpath = '//boxscore')
    game_pk <- xml_attr(x = root_boxscore, name = 'game_pk')
    
    dir_inning <- paste(dir_base, '/inning/inning_all.xml', sep = '')
    raw_inning <- xml(x = dir_inning)
    root_game <- xml_node(x = raw_inning, xpath = '//game')
    
    l_innings <- xml_children(root_game)
    atbat_df <- data.table()
    
    for (inning in l_innings) {
        i_num <- xml_attr(x = inning, name = 'num')
        l_inning_part <- xml_children(inning)
        for (inning_part in l_inning_part) {
            i_part <- xml_tag(inning_part)
            l_atbats <- xml_children(inning_part)
            l_atbats <- l_atbats[names(l_atbats) == 'atbat']
            if (length(l_atbats) > 0) {
                new_atbat <- data.table(game_pk = game_pk,
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
                atbat_df <- rbindlist(list(atbat_df, new_atbat))
            }
            
        }
    }
    return(atbat_df)
}
parse_atbats(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1')
class(parse_atbats(dir_base = './data/day_01/gid_2014_06_01_colmlb_clemlb_1'))


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

# Probability Scouting Report EDA
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






# XPath Syntax: http://www.w3schools.com/xpath/xpath_syntax.asp