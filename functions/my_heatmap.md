

ord = order(mtcars$mpg, decreasing=TRUE)
mcars = as.matrix(mtcars[ord,])
x = mcars
# matrix dimensions
di <- dim(x)
nr <- di[1L]
nc <- di[2L]
# row and column means
Rowv <- rowMeans(x)
Colv <- colMeans(x)
# row and col indices
rowInd <- 1L:nr
colInd <- 1L:nc
x <- x[rowInd, colInd]
# row and col names
labRow <- rownames(x)
labCol <- colnames(x)

# scale by "columns"
x <- sweep(x, 2L, colMeans(x), check.margin = FALSE)
sx <- apply(x, 2L, sd)
x <- sweep(x, 2L, sx, "/", check.margin = FALSE)  

# layout matrix
lmat <- rbind(c(NA, 3), 2:1)
# widths
lwid <- c(0.05, 4)
# heights
lhei <- c(0.05, 4)
lmat[is.na(lmat)] <- 0
margins = c(5, 2)

# graphic device
#dev.hold()
#on.exit(dev.flush())
#op <- par(no.readonly = TRUE)
#on.exit(par(op), add = TRUE)
# set layout
layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
par(mar = c(margins[1L], 2, 2, margins[2L]))
x <- t(x)
iy <- nr:1
x <- x[, iy]
# call image
image(1L:nc, 1L:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
  c(0, nr), axes = FALSE, xlab = "", ylab = "", col=brewer.pal(9,"YlGnBu"))
# add axes and labels
axis(1, 1L:nc, labels = labCol, las = 2, line = -0.5, tick = 0, 
     cex.axis = 0.2 + 1/log10(nc))
axis(2, iy, labels = labRow, las = 2, line = -0.5, tick = 0, 
     cex.axis = 0.2 + 1/log10(nr))
# add title
mtext(text="Heatmap", side=3, at=-0.3, line=0.5, cex=1.5, col="gray30", font=2)
# add grid
abline(h=1L:nr + 0.5, v=1L:nc + 0.5, col="white", lwd=1.7)
# par
par(mar = c(margins[1L], 0, 0, 0))
par(mar = c(0, 0, 0, margins[2L]))
