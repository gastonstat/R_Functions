

count_vowels <- function(string) {
  vowels <- c('a', 'e', 'i', 'o', 'u')
  split_chars <- unlist(strsplit(string, ''))
  counts <- rep(0, 5)
  for (v in seq_along(vowels)) {
    counts[v] <- sum(split_chars == vowels[v])
  }
  names(counts) <- vowels
  return(counts)
}

count_vowels("hola que tal")



split_chars <- function(x) {
  unlist(strsplit(x, ''))
}

a <- split_chars("hola que tal")

num_vowels <- function(chars) {
  vowels <- c('a', 'e', 'i', 'o', 'u')
  counts <- rep(0, 5)
  for (v in seq_along(vowels)) {
    counts[v] <- sum(chars == vowels[v])
  }
  names(counts) <- vowels
  counts
}

count_vowels <- function(string) {
  chars <- split_chars(string)
  num_vowels(chars)
}

count_vowels("hola que tal")
count_vowels("concepts in computing with data")
count_vowels("The quick brown fox jumps over the lazy dog")
count_vowels("hl q tl A E I O U")



num_vowels <- function(chars) {
  vowels <- c('a', 'e', 'i', 'o', 'u')
  counts <- rep(0, 5)
  for (v in seq_along(vowels)) {
    counts[v] <- sum(chars == vowels[v])
  }
  names(counts) <- vowels
  counts
}

count_vowels <- function(string) {
  chars <- split_chars(string)
  chars <- tolower(chars)
  num_vowels(chars)
}
count_vowels("hl q tl A E I O U")



library(lubridate)

sys_time <- Sys.time()
day <- weekdays(sys_time)
month <- months(sys_time)
sys_time_char <- as.character(sys_time)
sys_date <- strsplit(sys_time_char, " ")[[1]][1]
year <- substr(sys_date, start = 1, stop = 4)
day_num <- substr(sys_date, start = 9, stop = 10)
sprintf("Today is %s %s, %s %s", day, day_num, month, year)

today <- function() {
  sys_time <- Sys.time()
  day <- weekdays(sys_time)
  month <- months(sys_time)
  sys_time_char <- as.character(sys_time)
  sys_date <- strsplit(sys_time_char, " ")[[1]][1]
  year <- substr(sys_date, start = 1, stop = 4)
  day_num <- substr(sys_date, start = 9, stop = 10)
  sprintf("Today is %s %s, %s %s", day, day_num, month, year)
}

today()

tomorrow <- function() {
  next_day <- now() + days(1)
  day <- weekdays(next_day)
  month <- months(next_day)
  sys_time_char <- as.character(next_day)
  sys_date <- strsplit(sys_time_char, " ")[[1]][1]
  year <- substr(sys_date, start = 1, stop = 4)
  day_num <- substr(sys_date, start = 9, stop = 10)
  sprintf("Tomorrow is %s %s, %s %s", day, day_num, month, year)
}

tomorrow()

