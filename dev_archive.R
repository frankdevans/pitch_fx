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