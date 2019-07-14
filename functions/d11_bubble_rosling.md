---
layout: page
title: "Bubble-Chart"
---


# load colortools
library(colortools)
library(ggplot2)

# read data files
life = read.csv("data/life_expectancy.csv", header=TRUE, row.names=1)
mort = read.csv("data/mortality_rate.csv", header=TRUE, row.names=1)

# years
years = as.numeric(gsub("X", "", colnames(life)))

# colors from ggplot
n = 15
col_names = hcl(h=seq(15, 375-360/n, length=n)%%360, c=100, l=65)


# ========================================================
# Plot
# ========================================================
# prepare to save image
tiff("images/bubble_chart.tiff", width=650, height=470, res=80)
# set margins
op = par(mar = c(4, 3, 4, 1))
# call new plot
plot.new()
# define plot window
plot.window(xlim = c(50, 88),
            ylim = c(0, 140))
# add axis
axis(side=1, pos=0, col="gray50", at=seq(50, 85, 5), cex.axis=1)
axis(side=2, pos=50, las=2, lwd.ticks=0.5,
     col="gray50", cex.axis=1, line=0.1)
# add y-axis legend
mtext("Life Expectancy - Birth", side=1, line=2)
mtext("Mortality Rate", side=2, line=1.5)
# add legend
legend(82, 140, legend=rownames(life)[1:n], col=col_names, pt.cex=1, 
       pch=20, bty="n", cex=0.8, text.col="gray30")
# add data
for (i in 1:n)
{
  # define colors
  cols = sequential(col_names[i], 100/29, alpha=0.6, fun="log", plot=FALSE)
  # add line
  lines(life[i,], mort[i,], pch=20, col=cols[20])
  # add points
  size = as.numeric(log(life[i,] / 10))
  points(life[i,], mort[i,], pch=20, col=cols, cex=1.5)
}
# add title
mtext("Bubble Chart", side=3, at=52, line=1.5, col="gray20", cex=1.5, font=2)
mtext("Period: 1980 - 2008", side=3, at=67, line=-0.1, cex=1, col="gray30")
par(op)
# close device
dev.off()
