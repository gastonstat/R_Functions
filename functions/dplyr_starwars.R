
setwd("/Users/Gaston/Documents/stat133")

sw <- read.csv("data/starwars.csv", header = TRUE)

sw


sw %>% 
  group_by(gender, jedi) %>% 
  select(height, weight) %>%
  summarise(
    hgt = mean(height, na.rm = TRUE),
    wgt = mean(weight, na.rm = TRUE))


sw %>% 
  group_by(gender, jedi, species) %>% 
  select(height, weight) %>%
  summarise(
    hgt = mean(Height, na.rm = TRUE),
    wgt = mean(Weight, na.rm = TRUE))


# min and max height by gender
sw %>% 
  group_by(gender) %>%
  select(height) %>%
  summarise(
    minhgt = min(height, na.rm = TRUE),
    maxhgt = max(height, na.rm = TRUE)
  )


# max height of male
sw %>% 
  filter(gender == 'male') %>%
  select(height) %>%
  summarise(
    maxhgt = max(height, na.rm = TRUE)
    )


# max height of human male
sw %>% 
  filter(gender == 'male' & species == 'human') %>%
  select(height) %>%
  summarise(
    maxhgt = max(height, na.rm = TRUE)
  )

# equivalently
max(sw[sw$species=='human' & sw$gender=='male','height'])
