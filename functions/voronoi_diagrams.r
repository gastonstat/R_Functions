
install.packages("tripack")
library(tripack)

install.packages("alphahull")
library(alphahull)

data(tritest)
# Delaunay triangulation and Voronoi diagram calculation
x <- matrix(runif(20), nc = 2)
# Delaunay triangulation and Voronoi diagram calculation
delvor.obj <- delvor(x)
plot(delvor.obj)


n <- 300
theta<-runif(n,0,2*pi)
r<-sqrt(runif(n,0.25^2,0.5^2))
x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))
# Value of alpha
alpha <- 0.1
# alpha-shape 
ashape.obj <- ashape(x, alpha = alpha)
# Plot alpha-shape in blue, sample points in black, 
# and Delaunay triangulation in red
plot(ashape.obj, wlines= "del", col = c(4, 1, 2))



# plot a random mosaic
plot(voronoi.mosaic(runif(100),runif(100), duplicate="remove"))



data(tritest)
tritest.vm <- voronoi.mosaic(tritest$x,tritest$y)
tritest.cells <- cells(tritest.vm)
# higlight cell 12:
plot(tritest.vm)

trixy = do.call("cbind", tritest)
plot(trixy)


tr<-tri.mesh(tritest$x,tritest$y)
convex.hull(tr,plot.it=TRUE)
# random points:
rand.tr<-tri.mesh(runif(10),runif(10))
plot(rand.tr)
rand.ch<-convex.hull(rand.tr, plot.it=TRUE, add=TRUE, col="red")


data(quakes)
quakes.part<-quakes[(quakes[,1]<=-17 & quakes[,1]>=-19.0 &
                       quakes[,2]<=182.0 & quakes[,2]>=180.0),]
quakes.tri<-tri.mesh(quakes.part$lon, quakes.part$lat, duplicate="remove")
plot(quakes.tri)
convex.hull(quakes.tri, plot.it=TRUE, add=TRUE, col="red")


trixy = do.call("cbind", quakes.tri[2:3])
plot(trixy)
