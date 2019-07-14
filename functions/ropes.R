
setwd("/Users/Gaston/Documents/stat133")

# read data
ropes <- read.table("data/dynamicropes.txt", 
             header = TRUE, row.names = 1)

# load packages
library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(dplyr)

ggplot(ropes, 
       aes(x = Diameter, y = Weight, 
           color = Brand, label = rownames(ropes))) +
  geom_text() 


# Brand frequencies
ropes %>% 
  group_by(Brand) %>% 
  summarise(length(Brand))

# Avg diameter by brand
ropes %>%
  select(Brand, Diameter) %>%
  group_by(Brand) %>% 
  summarise(avgdiam = mean(Diameter, na.rm = TRUE))

# Avg diameter and weight by brand
avg_diam_weigh <- ropes %>%
  select(Brand, Diameter, Weight) %>%
  group_by(Brand) %>% 
  summarise(
    avgdiam = mean(Diameter, na.rm = TRUE),
    avgweig = mean(Weight, na.rm = TRUE))

ggplot(avg_diam_weigh, 
       aes(x = avgdiam, y = avgweig, label = Brand)) +
  geom_text()



# prcomp or princomp

# principal components analysis
ropes.pca = PCA(ropes[,2:7])
xs = ropes.pca$ind$coord[,1]
ys = ropes.pca$ind$coord[,2]


# vector of colors
brandcol <- c("antiquewhite4", "#00CCFFFF", "#CCFF00FF", "tan", "red", "black", 
    "tomato", "magenta", "orange", "blue", "#FFCC00FF", "yellow", "yellowgreen")
# ropes by name
pdf("D:/GoogleSites/ClimbingRopes/DynamicRopeNames.pdf", width=8, height=8)
plot(xs, ys, type="n", xlim=c(-4,6), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray93")
rect(-5, -5, 7, 4, col="gray90")
box(col="gray70", lwd=1)
mtext("Climbing Ropes (Spring 2011)", side=3, line=.5)
mtext(c("heavy duty","less","more"), side=1, at=c(.7,-2.9,4.2), line=c(.8,.7,.7), cex=c(1,.8,.8))
mtext(c("elasticity","more","less"), side=2, at=c(-.4,3,-3.6), line=1, cex=c(1,.8,.8))
abline(h=0, v=0, lty=2, col="gray80")
for (i in 1:nlevels(ropes$Brand))
{
    brand <- which(ropes$Brand==levels(ropes$Brand)[i])
    text(xs[brand], ys[brand], labels=rownames(ropes)[brand], cex=.8, 
         col=brandcol[i])
}
legend("bottomright", legend=levels(ropes$Brand), pch=15, cex=.7, 
    col=brandcol, bty="n", text.col="gray60")
dev.off()

# plot ropes by brand
pdf("D:/GoogleSites/DynamicRopeBrands.pdf", width=8, height=8)
plot(xs, ys, type="n", xlim=c(-4,5), xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
#rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="gray90")
rect(-6, -5, 7, 4, col="gray90")
box(col="gray70", lwd=1)
mtext("Climbing Ropes by Brand (Spring 2011)", side=3, line=.5)
mtext(c("heavy duty","less","more"), side=1, at=c(.7,-2.9,4.2), line=c(.8,.7,.7), cex=c(1,.8,.8))
mtext(c("elasticity","more","less"), side=2, at=c(-.4,3,-3.6), line=1, cex=c(1,.8,.8))
abline(h=0, v=0, lty=2, col="grey")
for (i in 1:nlevels(ropes$Brand))
{
    brand <- which(ropes$Brand==levels(ropes$Brand)[i])
    text(xs[brand], ys[brand], labels=levels(ropes$Brand)[i], cex=.8, 
         col=brandcol[i])
}
dev.off()


plot(1:8, rep(1,8), type="n", ylim=c(1,3))
points(1:8, rep(1,8), pch=19, col=brewer.pal(8, "Dark2"), cex=2)
points(1:8, rep(2,8), pch=19, col=brewer.pal(8, "Set1"), cex=2)



# ==================== using ggplot2 ==========================
# add dim1 and dim2
ropes$pc1 = xs
ropes$pc2 = ys

# first plot
ggplot(data=ropes, aes(x=pc1, y=pc2, colour=Brand, label=rownames(ropes))) +
geom_text(alpha=.5) + xlim(-4, 5.5) +
scale_colour_manual(values=brandcol)
    

ggplot(data=ropes, aes(x=pc1, y=pc2, color=Brand, label=rownames(ropes))) +
geom_text(aes(size=Falls)) + xlim(-4, 5.5) +
scale_colour_manual(values=brandcol) +
opts(title="Climbing Ropes (Spring 2011)")

ggplot(data=ropes, aes(x=pc1, y=pc2, color=Brand, label=rownames(ropes))) +
geom_text(aes(size=Diameter)) +
scale_colour_manual(values=brandcol) +
opts(title="Climbing Ropes  (Spring 2011)")

# ========================================================================================


# Diameter
barplot(table(cuerdas$diameter), main="Distribution of dynamic rope diameters (mm)", 
         cex.main=1, col=heat.colors(23)[21:1], border=NA, ylim=c(0,15),
        xlab="diameter", ylab="frequency", cex.lab=.8, cex.axis=.7, cex.names=.7)
abline(h=0)
abline(h=seq(2,14,2), col="grey")
barplot(table(cuerdas$diameter), col=heat.colors(23)[21:1], border=NA, ylim=c(0,15),
         xlab="diameter", ylab="frequency", cex.lab=.8, cex.axis=.7, cex.names=.7, add=T)

# Weight
barplot(table(cuerdas$weight), main="Distribution of dynamic rope weights  (grams/meter)",
        cex.main=1, col=heat.colors(27)[25:1], border=NA, 
         xlab="weight", ylab="frequency", cex.lab=.8, cex.axis=.7, cex.names=.7)
abline(h=0)
abline(h=seq(2,10,2), col="grey")
barplot(table(cuerdas$weight), col=heat.colors(27)[25:1], border=NA,
         xlab="weight", ylab="frequency", cex.lab=.8, cex.axis=.7, cex.names=.7, add=T)

ropes = data.frame(cuerdas)

ggplot(ropes, aes(diameter)) +
geom_histogram(binwidth=.25, colour="white", fill=brewer.pal(9,"PuRd")[6])

ggplot(ropes, aes(weight)) +
geom_histogram(binwidth=2.5, colour="white", fill="steelblue")



# Impact force
barplot(table(cuerdas$FC), main="Distribution  of  Impact  Force", 
        cex.main=1, cex.axis=.7, cex.names=.6, col=heat.colors(32)[30:1], 
        border=NA, ylim=c(0,10))
abline(h=0)
abline(h=seq(2,10,2), col="grey")
barplot(table(cuerdas$FC), cex.axis=.7, cex.names=.6, col=heat.colors(32)[30:1], 
        border=NA, ylim=c(0,10), xlab="Impact Force (kN)", ylab="frequency", add=T,
        cex.lab=.8)

