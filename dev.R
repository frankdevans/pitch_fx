



# Load Flat Dims
dim_stadium <- read.table(file = './data/dim_stadium.csv',
                          header = TRUE,
                          sep = ',',
                          stringsAsFactors = FALSE)

plot(dim_stadium$longitude, dim_stadium$latitude)


elias_lahman <- read.table(file = './data/elias_to_lahman.csv',
                           header = TRUE,
                           sep = ',',
                           stringsAsFactors = FALSE)

length(unique(elias_lahman$eliasid)) / nrow(elias_lahman)
length(unique(elias_lahman$lahmanid)) / nrow(elias_lahman)