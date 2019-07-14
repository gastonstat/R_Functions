# Mexico's Metro Map
# Designed by Stephen Few, a bullet chart 
# A variation on a bar chart, bullet charts compare a given 
# quantitative measure (such as profit or revenue) against 
# qualitative ranges (e.g., poor, satisfactory, good) and 
# related markers (e.g., the same measure a year ago).

# ========================================================
# Data
# ========================================================
# Mexico Metro
setwd("/Users/gaston/Documents/Gaston/GotPlot/data/")

datos = read.csv("MexicoMetro.csv", row.names=1, stringsAsFactors=FALSE)

# vector of colors
cols = c("pink2", "steelblue", "khaki4", "paleturquoise3",
         "gold", "red", "orange", "turquoise4", "brown", "maroon", "olivedrab")

# colors in hsv
cols.hsv = rgb2hsv(col2rgb(cols))
cols.hsva = rep("", 11)
txt.hsva = rep("", 11)
for (j in 1:ncol(cols.hsv))
{
  a = cols.hsv[,j]
  cols.hsva[j] = hsv(h=a[1], s=a[2], v=a[3], alpha=0.3)
  txt.hsva[j] = hsv(h=a[1], s=a[2], v=a[3], alpha=0.7)
}


# ========================================================
# Plot
# default: plot(datos$lon, datos$lat,
# ========================================================
# prepare to plot
setwd("/Users/gaston/Documents/Gaston/GotPlot/images")
tiff("metro_mexico.tiff", width=750, height=550)
# set margins and background color
op = par(mar = c(1.5,1.5,1.5,1.5), bg="gray13")
# define plot
plot(datos$lon, datos$lat, type="n", xaxt="n", yaxt="n", 
     bty="n", xlab="", ylab="")
# plotting metro stations
for (i in 1:11)
{
  # station names
  text(datos$lon[datos$linea==i], datos$lat[datos$linea==i],
       datos$nombre[datos$linea==i], col=txt.hsva[i], cex=0.8)
  # metro lines
  lines(datos$lon[datos$linea==i], datos$lat[datos$linea==i], 
        col=cols.hsva[i])
  # metro stations
  points(datos$lon[datos$linea==i], datos$lat[datos$linea==i], 
         col=cols.hsva[i], pch=20, cex=sqrt(datos$afluencia[datos$linea==i])/1200)
}
# add title
title("Annual ridership in Mexico City Metro - 2011",
      col.main="gray85", cex.main=1)
# add legend
text(-98.983, 19.495, col="gray70",
     labels="Millions \nof pasangers", cex=0.9)	
text(rep(-98.988,6), seq(19.48, 19.42, length=6), col="gray70",
     labels=round(summary(datos$afluencia/1000000)), cex=0.8)
points(rep(-98.976,6), seq(19.48, 19.42, length=6), pch=20,
       cex=summary(sqrt(datos$afluencia)/1200), col="gray70")  
# reset default graphic parameters
par(op)
# turn off device
dev.off()
