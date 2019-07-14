# Data from: CDC 
# Centers for Disease Control and Prevention
# https://data.cdc.gov/
#
# U.S. and State Trends on Teen Births, 1990 - 2013
# Final birth data for women aged 15-19, 15-17, and 18-19 
# for the United States and each of the 50 states.
# https://data.cdc.gov/Health-Statistics/U-S-and-State-Trends-on-Teen-Births-1990-2013/y268-sna3
#
# Licensing attribution:
# Data Provided By National Center for Health Statistics
# License: Public Domain


# download a copy of the data to your computer
download.file(
  url = 'https://data.cdc.gov/api/views/y268-sna3/rows.csv',
  destfile = '~/Documents/stat133/teenbirths.csv')

library(ggplot2)
library(FactoMineR)

# read teen-births
tb <- read.csv('~/Documents/stat133/teenbirths.csv', 
               nrows = 1173)
head(tb)
tail(tb)

# Year as factor
tb$Year <- factor(tb$Year)
years <- levels(tb$Year)

# add state abbreviation
short_name <- c(
  'AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 
  'GA', 'HI', 'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 
  'MD', 'MA', 'MI', 'MN', 'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 
  'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA', 'RI', 
  'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 
  'WY')

tb$Short <- rep(short_name, nrow(tb) / length(short_name))


# birth matrix
# sample of table constructed from a set of given years
bm <- matrix(NA, nlevels(tb$State), nlevels(tb$Year))
for (y in seq_along(levels(tb$Year))) {
  bm[ ,y] <- tb$State.Rate[tb$Year == years[y]]
}
rownames(bm) <- short_name
colnames(bm) <- years

bm[1:10, 1:5]


# ----------------------------------------------------
# US Birth Rate
# ----------------------------------------------------

birth_rate <- tapply(tb$U.S..Birth.Rate, tb$Year, FUN = unique)

us_birth_rate <- data.frame(
  year = years,
  birth_rate = birth_rate)

# timeline
ggplot(us_birth_rate, aes(year, birth_rate, group = 1)) +
  geom_line(color = "#888888aa", size = 2) +
  geom_point() +
  ylim(0, max(birth_rate)) +
  ggtitle("U.S. Birth Rate (15-19)")


# ----------------------------------------------------
# Selected States Birth Rate
# ----------------------------------------------------

sub_rate <- subset(
  tb, 
  State %in% c('California', 'Texas', 'New York'),
  select = c('Year', 'State.Rate', 'State'))

# timeline
ggplot(sub_rate, aes(Year, State.Rate, group = State)) +
  geom_line(aes(color = State), size = 2) +
  geom_point() +
  ggtitle("Birth Rate (15-19) selected states")



# timeline
ggplot(tb, aes(Year, State.Rate, group = State)) +
  geom_line(color = 'gray50', alpha = 0.5) +
  geom_line(data = us_birth_rate, 
            aes(year, birth_rate, group = 1),
            color = "#F9508B", size = 1.5, alpha = 0.7) +
  ggtitle("State Birth Rate (15-19)")


# ----------------------------------------------------
# U.S. Birth Rate: Boxplots per year
# ----------------------------------------------------

# timeline
ggplot(tb, aes(Year, State.Rate)) +
  geom_boxplot() +
  ggtitle("U.S. Birth Rate (15-19)") +
  geom_line(data = us_birth_rate, 
            aes(year, birth_rate, group = 1),
            color = "#F9508B", size = 1.5, alpha = 0.7)


# ----------------------------------------------------
# U.S. Birth Rate: Boxplots per year
# ----------------------------------------------------

# barchart of rates below and above U.S. rate
tb_1990 <- subset(tb, Year == 2012)
rate_diff <- tb_1990$State.Rate - tb_1990$U.S..Birth.Rate
col_1990 <- character(length(rate_diff))
col_1990[rate_diff < 0] <- "#CE4B6A"
col_1990[rate_diff >= 0] <- "#4b6ace"

op <- par(mar = c(2.5, 5, 1, 1))
barplot(sort(rate_diff), names.arg = tb_1990$State[order(rate_diff)],
        horiz = TRUE, las = 1, col = col_1990[order(rate_diff)],
        border = NA, cex.names = 0.5)
par(op)



# ----------------------------------------------------
# Constructing tables
# ----------------------------------------------------

# write a function that given a set of years (at least 2 years)
# produces a data frame with the birth rates of all the states
get_table <- function(data = tb, years) {
  if (length(years) <= 1) {
    stop('years must be of length 2 or more')
  }
  all_years <- levels(data$Year)
  good_years <- years %in% all_years
  if (all(good_years)) {
    states <- levels(data$State)
    num_years <- length(years)
    output <- matrix(NA, length(states), num_years)
    for (j in 1:length(years)) {
      output[ ,j] <- data$State.Birth[data$Year == years[j]]
    }
    rownames(output) <- states
    colnames(output) <- years
  } else {
    stop(paste('\ninvalid years: ', years[!good_years]))
  }
  output
}

y = get_table(tb, years = c(1990, 1991))
head(y)
tail(y)


all(c(1989, 1991) %in% years)


# write a function that given the name of a state,
# a data frame is produced with: Year and State.Rate
subset(tb, State == 'Alabama', select = c(Year, State.Rate))



# ----------------------------------------------------
# PCA and dimension reduction
# ----------------------------------------------------

# PCA
bm_pca <- PCA(bm[ ,11:23], graph = FALSE)
plot(bm_pca, choix = 'ind')
plot(bm_pca, choix = 'var')

# MDS
sizes <- rowMeans(bm)
bm_dist <- dist(bm[ ,11:23])
mds <- cmdscale(bm_dist)

plot(mds, type = 'n')
abline(h = 0, v= 0, col = 'gray80')
points(mds[, 1], mds[, 2], cex = sizes / 10,
       col = '#ccccccdd', pch = 19, xpd = TRUE)
text(mds[, 1], mds[, 2], cex = 0.7, col = 'gray30',
     labels = levels(factor(tb$Short)))
