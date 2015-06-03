library(pitchRx)


t_pfx <- scrape(start = '2014-06-01', end = '2014-06-01')
names(t_pfx)

View(t_pfx$po)
View(t_pfx$atbat)
View(t_pfx$action)
View(t_pfx$runner)
View(t_pfx$pitch)























