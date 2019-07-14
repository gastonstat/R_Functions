
library(ggplot2)
library(ca)
library(FactoMineR)
library(MASS)

# set working directory
setwd("D:/GoogleSite/YelpBerkeleyRestaurants")

# read data
rest = read.csv("restaurants2.csv", header=TRUE, row.names=1, stringsAsFactors=FALSE)

# restaurants without reviews and restaurant in North Berkeley Hills
no.revs = which(is.na(rest$Reviews))
no.neig = c("North Berkeley Hills", "Gourmet Ghetto", "East Solano Ave", 
   "Telegraph Ave", "Elmwood")
nbh = which(rest$Neighborhood %in% no.neig)
rest = rest[-c(no.revs, nbh),]

# subset data without UC Campus Area
uc.campus = which(rest$Neighborhood=="UC Campus Area")
subrest = rest[-uc.campus,]

# remove duplicated restaurants in UC campus Area
rest.campus = subset(rest, Neighborhood=="UC Campus Area")
no.ucarea = intersect(rest$Name[rest$Neighborhood=="Downtown Berkeley"],
   rest$Name[rest$Neighborhood=="UC Campus Area"])
no.uc = which(rest.campus$Name %in% no.ucarea)
rest.campus = rest.campus[-no.uc,]

# clean dataset
rest = rbind(subrest, rest.campus)
#write.csv(rest, "BerkeleyRestaurants.csv")

# stars rating table
stars.table = with(rest, table(factor(Stars)))
barplot(stars.table, border=NA, main="Ratings Distribution",
   xlab="Rating Stars", ylab="Frequency", cex.main=1)

# restaurants per neighborhood
neig.table = with(rest, table(factor(Neighborhood)))
neig.table = sort(neig.table)
par(mar=c(5,10,4,2))
barplot(neig.table, border=NA, main="Restaurants per Neighborhood",
   xlab="Frequency", cex.main=1, horiz=TRUE, las=2, xlim=c(0,150))

# ratings per neighborhood
starsneig.table = with(rest, table(factor(Neighborhood), factor(Stars)))
starsneig.ca = ca(starsneig.table)
plot(starsneig.ca)

# what neighborhood has the most 5.0 stars?
neig.propstars = prop.table(starsneig.table, 1)
neigh5s = sort(round(neig.propstars[,9], 3))
par(mar=c(5,10,4,2))
barplot(neigh5s, border=NA, las=2, xlab="Proportion",
    main="Proportion of 5 stars restaurants per Neighborhodd",
    horiz=TRUE, xlim=c(0,.1))

# Neighborhoods with restaurants below 2.5 stars?
neighlows = sort(round(rowSums(neig.propstars[,1:4]), 3))
par(mar=c(5,10,4,2))
barplot(neighlows, border=NA, las=2, xlab="Proportion",
    main="Proportion of restaurants below 2.5 Stars per Neighborhodd",
    horiz=TRUE, xlim=c(0,.2), cex.main=1)

# Top 20 reviewed restaurants
rest.revs = with(rest, sort(as.numeric(Reviews), decreasing=TRUE))
top.revs = with(rest, order(as.numeric(Reviews), decreasing=TRUE))
#top20revs = 
cbind(rest$Name[top.revs[1:20]], rest$Neighborhood[top.revs[1:20]], 
   rest$Reviews[top.revs[1:20]])

# Price and neighborhood
priceneig.table = with(rest, table(factor(Neighborhood), factor(Price)))
neig.price = prop.table(priceneig.table, 1)
# inexpensive restaurants
neigcheap = sort(round(neig.price[,1], 3))
par(mar=c(5,10,4,2))
barplot(neigcheap, border=NA, las=2, xlab="Proportion",
    main="Proportion of inexpensive($) restaurants per Neighborhodd",
    horiz=TRUE, xlim=c(0,.9), cex.main=1)
# pricy restaurants
neigexp = sort(round(rowSums(neig.price[,3:4]), 3))
par(mar=c(5,10,4,2))
barplot(neigexp, border=NA, las=2, xlab="Proportion",
    main="Proportion of expensive($$$-$$$$) restaurants per Neighborhodd",
    horiz=TRUE, xlim=c(0,.15), cex.main=1)



rest1 = with(rest, data.frame(factor(Neighborhood), factor(Stars), 
  factor(Credit), factor(Price), factor(Alcohol), factor(Meal), factor(Reservation)))
rest.mca = mca(rest1)
rest.MCA = MCA(rest1[complete.cases(rest1),])


# neighborhood and type of meal
with(rest, table(factor(Neighborhood[grep("Breakfast", Meal)])))
with(rest, table(factor(Neighborhood[grep("Brunch", Meal)])))
with(rest, table(factor(Neighborhood[grep("Lunch", Meal)])))
with(rest, table(factor(Neighborhood[grep("Dinner", Meal)])))
with(rest, table(factor(Neighborhood[grep("Night", Meal)])))

