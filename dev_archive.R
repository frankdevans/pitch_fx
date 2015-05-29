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