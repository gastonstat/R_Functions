
library(XML)
library(stringr)

url <- "http://en.wikipedia.org/wiki/List_of_Super_Bowl_champions"
x <- readHTMLTable(url, stringsAsFactors = FALSE)

length(x)

# 2nd table SuperBowl
sb <- x[[2]]

# get year
year <- substr(sb$Date, start = 2, stop = 5)
year <- as.numeric(year[1:49])

sb_champ <- sb$'Winning team'
sb_champ <- sb_champ[1:49]
winner <- unlist(str_extract_all(sb_champ, "^(\\w+.\\.? ){1,3}"))
winner <- sub("[[:digit:]]{2} $", "", winner)
winner <- gsub("\\s+$", "", winner)
winner


sb_losing <- sb$'Losing team'
sb_losing <- sb_losing[1:49]
loser <- unlist(str_extract_all(sb_losing, "^(\\w+.\\.? ){1,3}"))
loser <- sub("[[:digit:]]{2} $", "", loser)
loser <- gsub("\\s+$", "", loser)
loser



# str_extract(team, '\\w+$')

save(year, winner, loser, file = "superbowl_champions.RData")

