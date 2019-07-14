# download file
download.file(
  url = 'https://data.cdc.gov/api/views/vp9c-m6nq/rows.csv',
  destfile = '~/Documents/stat133/health_1980_2009.csv'
)

library(readr)

#
health <- read_csv(
  file = '~/Documents/stat133/health_1980_2009.csv',
  col_types = 'dcic_d')

dim(health)
names(health)

head(health)

table(health$Year)
table(health$Group)

all_health <- subset(health, Group == 'All persons')
dcauses <- all_health$`Cause of death` != 'All causes'
all_health <- all_health[dcauses, ]

barplot(all_health$Deaths[1:10], horiz = TRUE)

causes <- rbind(
  all_health$Deaths[1:10],
  all_health$Deaths[11:20])

op <- par(mar = c(3, 14, 1, 1))
barplot(causes, horiz = TRUE, beside = TRUE, 
        names.arg = all_health$`Cause of death`[1:10],
        las = 2)
par(op)
