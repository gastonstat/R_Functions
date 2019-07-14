
setwd("/Users/gaston/Documents/R_package_arcdiagram/starwars/StarWars/Text_files")
setwd("/Users/gaston/Documents/R_package_arcdiagram/starwars/StarWars/Rscripts")
setwd("/Users/gaston/Documents/R_package_arcdiagram/starwars/StarWars/Text_files")


polygono <- function (x, y, siz = siz, col = "black", border = NULL, K = 5, 
          startalph = -45, ...) 
{
  if (missing(col)) {
    col = rep(1, length = length(x))
  }
  if (missing(siz)) {
    siz = rep(0.3, length = length(x))
  }
  if (missing(border)) {
    border = rep(1, length = length(x))
  }
  if (missing(K)) {
    K = 5
  }
  if (K < 3) {
    K = 3
  }
  if (length(siz) == 1) {
    siz = rep(siz, length = length(x))
  }
  if (length(col) == 1) {
    col = rep(col, length = length(x))
  }
  if (length(border) == 1) {
    border = rep(border, length = length(x))
  }
  if (missing(startalph)) {
    startalph = -45
  }
  up = par("usr")
  ui = par("pin")
  ratx = (up[2] - up[1])/ui[1]
  raty = (up[4] - up[3])/ui[2]
  p.x = vector()
  p.y = vector()
  alph = (360/K) * pi/180
  stalph = startalph * pi/180
  angs = stalph + seq(from = 0, to = 2 * pi, by = alph)
  phi = stalph + seq(from = 0, to = 2 * pi, by = alph)
  for (i in 1:length(x)) {
    x0 = x[i]
    y0 = y[i]
    usizx = siz[i] * ratx
    usizy = siz[i] * raty
    p.x = x0 + (usizx * cos(phi) + usizx * sin(phi))
    p.y = y0 + (-usizy * sin(phi) + usizy * cos(phi))
    if (!is.null(col[i])) {
      polygon(p.x, p.y, col = col[i], border=NA)
    }
  }
}




install.packages("GEOmap")
library(GEOmap)

# plot polygon
n = 50
set.seed(n)
x = rnorm(n)
y = rnorm(n)
sizes = runif(n, 0.1, 0.3)
saturations = runif(n, 0.55, 0.80)
degrees = sample(c(10,20,30,40), size=n, replace=TRUE)

pcolors = hsv(saturations, 1, 1, alpha=0.3)

op = par(mar = rep(0,4), bg = "gray5")
plot(x, y, type="n")
for (i in 1:50) {
  polygono(x[i],  y[i], siz=sizes[i], col=pcolors[i], border=NA, 
       K=15, startalph = degrees[i])  
}
par(op)



op = par(mar = rep(0,4), bg = "gray5")
plot(x, y, type="n")
polygono(x[5], y[5], siz=0.5, col=pcolors[5], border=pcolors[5], 
     K=20, startalph = degrees[5])
xoff = seq(x[5], x[5]+3, by=0.05)
small = seq(0.5, 0.3, length.out=length(xoff))
verti = seq(y[5], 0.5, length.out=length(xoff))
for (k in seq_along(xoff)) {
  for (h in seq_along(verti)) {
    polygono(xoff[k], verti[h], siz=small[k], col=pcolors[5], border=NA, 
             K=20, startalph = degrees[5])    
  }
}
par(op)
polygono(x[5],  y[5], siz=0.5, col="#3B00FFbb", border=NA, 
         K=20, startalph = degrees[5])
polygono(x[1],  y[1], siz=0.5, col=pcolors[1], border=NA, 
         K=20, startalph = degrees[1])
par(op)


