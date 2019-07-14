

# load library maps and ggplot2
library(maps)
library(ggplot2)

# NOAA url to get datafiles by Basin
noaa = "ftp://eclipse.ncdc.noaa.gov/pub/ibtracs/v03r03/wmo/csv/basin/"

# vector of basins 'NA' and 'EP'
basin = c("NA", "EP")

# read files (Basins 'NA' and 'EP')
NA.basin = read.csv(paste(noaa, "Basin.", basin[1], ".ibtracs_wmo.v03r03.csv",
    sep=""), skip=1, stringsAsFactors=FALSE)
EP.basin = read.csv(paste(noaa, "Basin.", basin[2], ".ibtracs_wmo.v03r03.csv",
    sep=""), skip=1, stringsAsFactors=FALSE)


# remove variable information
NA.basin = NA.basin[-1,]
EP.basin = EP.basin[-1,]

# setting format in NA.basin
NA.basin$Season = as.numeric(NA.basin$Season)
NA.basin$Latitude = as.numeric(gsub("^ ", "", NA.basin$Latitude))
NA.basin$Longitude = as.numeric(gsub("^ ", "", NA.basin$Longitude))
NA.basin$Wind.WMO. = as.numeric(gsub("^ ", "", NA.basin$Wind.WMO.))
NA.basin$Pres.WMO. = as.numeric(gsub("^\\s+", "", NA.basin$Pres.WMO.))
NA.basin$Wind.WMO..Percentile = as.numeric(gsub("^\\s+", "", NA.basin$Wind.WMO..Percentile))
NA.basin$Pres.WMO..Percentile = as.numeric(gsub("^\\s+", "", NA.basin$Pres.WMO..Percentile))

# setting format in EP.basin
EP.basin$Season = as.numeric(EP.basin$Season)
EP.basin$Latitude = as.numeric(gsub("^ ", "", EP.basin$Latitude))
EP.basin$Longitude = as.numeric(gsub("^ ", "", EP.basin$Longitude))
EP.basin$Wind.WMO. = as.numeric(gsub("^ ", "", EP.basin$Wind.WMO.))

# extract month NA.basin
time.date = strsplit(NA.basin$ISO_time, " ")
iso.date = unlist(lapply(time.date, function(x) x[1]))
iso.month = substr(iso.date, 6, 7)
NA.basin$Month = factor(iso.month, labels=c(month.name))

# extract month EP.basin
time.date = strsplit(EP.basin$ISO_time, " ")
iso.date = unlist(lapply(time.date, function(x) x[1]))
iso.month = substr(iso.date, 6, 7)
EP.basin$Month = factor(iso.month, labels=c(month.name)[-4])


# join data frames
storms = rbind(NA.basin, EP.basin)

# world map
wm = map_data("world")

# select years 1999-2010
substorms = subset(storms, Season %in% 1999:2010)

# remove unnamed storms
nop = which(substorms$Name == "NOT NAMED")
substorms = substorms[-nop,]

# joining name and season
substorms$ID = as.factor(paste(substorms$Name, substorms$Season, sep="."))

# name as factor
substorms$Name = as.factor(substorms$Name)


# plot map 1 (option A)
ggplot(substorms, aes(x=Longitude, y=Latitude, group=ID)) +
geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
    fill='gray15', colour='gray10', size=0.2) +
geom_path(data=substorms, aes(group=ID, colour=Wind.WMO.), alpha=0.6, size=0.7) +
xlim(-138, -20) +
ylim(3, 55) +
labs(x="", y="", colour="Wind \n(knots)") +
opts(panel.background = theme_rect(fill='gray10', colour='gray30'),
    title = "Hurricane Trajectories 1999 - 2010 \nwww.ncdc.noaa.gov/oa/ibtracs",
    axis.text.x = theme_blank(),
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank())



# hurricanes by Month
ggplot(substorms, aes(x=Longitude, y=Latitude, group=ID)) +
geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
    fill='gray15', colour='gray10', size=0.2) +
geom_path(data=substorms, aes(group=ID, colour=Wind.WMO.), size=0.5) +
xlim(-138, -20) +
ylim(3, 55) +
labs(x="", y="", colour="Wind \n(knots)") +
facet_wrap(~ Month) +
opts(title = "Hurricane Trajectories by Month (1999 - 2010)",
    panel.background = theme_rect(fill='gray10', colour='gray30'),
    axis.text.x = theme_blank(),
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank())


# hurricanes by Year
ggplot(substorms, aes(x=Longitude, y=Latitude, group=ID)) +
geom_polygon(data=map_data("world"), aes(x=long, y=lat, group=group),
    fill='gray15', colour='gray10', size=0.2) +
geom_path(data=substorms, aes(group=ID, colour=Wind.WMO.), size=0.5) +
xlim(-138, -20) +
ylim(3, 55) +
labs(x="", y="", colour="Wind \n(knots)") +
facet_wrap(~ Season) +
opts(title = "Hurricane Trajectories by Year (1999 - 2010)",
    panel.background = theme_rect(fill='gray10', colour='gray30'),
    axis.text.x = theme_blank(),
    axis.text.y = theme_blank(),
    axis.ticks = theme_blank(),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank())

