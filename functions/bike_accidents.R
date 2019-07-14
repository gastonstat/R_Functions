
# source: Bike Accident Tracker 2.0 from 'The Bay Citizen'

# setting working directory
setwd("/Users/Gaston/Documents/stat133")

# load package XML
library(ggplot2)
library(dplyr)



# ========================= IMPORT DATA & CLEANSING ==========================

# codify empty strings as missing data (NA)
accidents <- read.csv("data/BikeAccidents.csv", header = TRUE, 
	stringsAsFactors = FALSE, na.strings = "")

# num of accidents by county, decreasing order
accidents %>%
  group_by(County) %>%
  summarise(freq = length(County)) %>%
  arrange(desc(freq))

# num and prop of accidents by county, decreasing order
accidents %>%
  group_by(County) %>%
  summarise(freq = length(County)) %>%
  mutate(prop = round(100 * freq / sum(freq), 2)) %>%
  arrange(desc(freq))



# split date and time
date_time = strsplit(accidents$DateTime, " ")
dates = sapply(date_time, function(x) x[1])
times = sapply(date_time, function(x) x[3])

## we need to reformat dates
# split in month, day, year
mdy = strsplit(dates, "/")
# get month, day, year
Month = sapply(mdy, function(x) x[1])
Day = sapply(mdy, function(x) x[2])
Year = paste("20", sapply(mdy, function(x) x[3]), sep="")
# as.Date format: year-month-date
Date = as.Date(paste(Year, Month, Day, sep="-"))

## reformat times
times = gsub("\\.", "", times)

## reformat City
city = accidents$City
cities = gsub("(Sherrif's Code|Sheriff's Code)", "", city)
cities = gsub(" $", "", cities)

# create address
Address = with(accidents, paste(Street1, " and ", 
	Street2, ", ", City, ", ", "CA", sep=""))

# add variables to data frame accidents
accidents$Date = Date
accidents$Year = Year
accidents$Month = factor(gsub("^0", "", Month), 
	levels=1:12, labels=month.name, ordered=TRUE)
accidents$Day = Day
accidents$Time = times
accidents$CityRe = cities
accidents$Address = Address

# exporting formatted data
write.csv(accidents, "BikeAccidentsClean.csv", row.names=FALSE)



# ====================== TABLES AND SUMMARY STATISTICS ======================

# codify empty strings as missing data (NA)
accidents = read.csv("BikeAccidentsClean.csv", header=TRUE, 
	stringsAsFactors=FALSE)

# single variable tables
sort(table(accidents$County), decreasing=TRUE)
sort(table(accidents$City), decreasing=TRUE)
sort(table(accidents$AtFault), decreasing=TRUE)
sort(table(accidents$Lighting), decreasing=TRUE)
sort(table(accidents$RoadSurface), decreasing=TRUE)
sort(table(accidents$Injured), decreasing=TRUE)
table(accidents$Month)

# combined tables
with(accidents, table(Month, Year))
with(accidents, table(Violation, Year))


# plots
ggplot(accidents, aes(x=AtFault, y=Injured)) +
geom_bar() +
facet_wrap(~ Year)


# ====================== DECISION TREE ======================

# load rpart
library(rpart)

NewInjured = accidents$Injured
NewInjured[NewInjured > 0] = 1
accidents$NewInjured = NewInjured
bikes.tree = rpart(NewInjured ~ AtFault + Month + SecondVehicle + County + Lighting, 
  data=accidents, parms=list(split='gini'))
plot(bikes.tree)
text(bikes.tree, use.n=TRUE)





# ========================= GEOCODING WITH GOOGLE MAPS =========================
# Geocoding: get location coordinates (longitude and latitude) from an address
# check google maps geocoding documentation at:
# https://developers.google.com/maps/documentation/geocoding/
# the limit of queries per day seems to be 2500

# google maps api geocode url
geo_url = "http://maps.googleapis.com/maps/api/geocode/xml?address="

# vector to store results
latitude = rep(NA, 2500)
longitude = rep(NA, 2500)

for (i in 3:2500)
{
	# address query for google maps
	address = with(accidents,
		paste(Street1[i], " and ", Street2[i], ", ", 
		CityRe[i], ", CA", sep=""))
	address = gsub(" ", "\\+", address)

	# using 'Place Autocomplete Requests'
	doc = htmlParse(paste(geo_url, address, "&sensor=false", sep=""))
		
	# extract latitude and longitude
	lat_tmp = xpathSApply(doc, "//location/lat", xmlValue)
	if (length(lat_tmp) > 0) latitude[i] = lat_tmp
	lon_tmp = xpathSApply(doc, "//location/lng", xmlValue)
	if (length(lon_tmp) > 0) longitude[i] = lon_tmp
}





# -----------------------------------------------------
# convert as numeric
num = 1:2500
lat = as.numeric(latitude)
lon = as.numeric(longitude)

latlon = cbind(num, lat, lon)
# import first 2500 reads
write.table(latlon, "latlon.txt", row.names=FALSE)
# import next 2500 reads
num = num + 2500
latlon = cbind(num, lat, lon)
write.table(latlon, "latlon.txt", row.names=FALSE, col.names=FALSE, append=TRUE)



#######################################################
# first latitude and longitude of address
# Newark+Bl+and+Mayhews+Landing+Rd,+Newark,+Alameda,+CA
lat1; long1
#######################################################
