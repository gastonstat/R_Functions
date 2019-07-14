# Network (les miserables)

# ========================================================
# Data
# ========================================================
# require igraph
library(igraph)

# read file
mis_graph = read.graph(file="/Users/gaston/Documents/Gaston/GotPlot/data/miserables_graph.gml", format="gml")

# get groups
groups = get.vertex.attribute(mis_graph, "group")

# get vertex names
names = get.vertex.attribute(mis_graph, "label")

# vertex colors palette
vpal = c("#9ca243", "#BEBADA", "#8DA0CB", "#5b6b8e", "#d3e4a6",
         "#afc568",  "#657845", "#f3d581", "#E6AB02", "#b69f60","#81632b")
# vertex border color palette
borders = c("#7c8135", "#858298", "#62708e", "#3f4a63", "#939f74",
            "#748e4b",  "#3a4a22", "#aa955a", "#a17701", "#7f6f43","#5a451e")
# vertex colors
vcols = vpal[groups+1]

# get edge width
ewidths = get.edge.attribute(mis_graph, "value")

# get numeric edgelist
aux_edgelist = get.edgelist(mis_graph)

# vertex sizes
col1 = aux_edgelist[,1]
col2 = aux_edgelist[,2]
vsize = rep(0, length(names))
for (i in 1:length(names))
{
  tmp = union(which(col1==i), which(col2==i))
  vsize[i] = sum(ewidths[tmp])
}
vcex = log(vsize)
vcex[vcex < 1] = 1

# colors
tmpcols = unique(as.vector(aux_edgelist))
ncols = vpal[groups[tmpcols]+1]

# reorder edgelist according to group
subord =  order(groups, decreasing=TRUE)
#subord_nodes = tmp[subord]
ng = length(unique(groups))
group_list = vector("list", ng)
group_sorted = sort(groups, decreasing=TRUE)
for (i in 1:ng)
{
  j = 11 - i
  which_sort = which(group_sorted == j)
  tmp = order(vcex[subord][which_sort], decreasing=TRUE)
  group_list[[i]] = which_sort[tmp]
}
neword = subord[unlist(group_list)]
ord = neword
ord_nodes = tmp[ord]

# give vertex names using the "name" attribute
V(mis_graph)$name = names
# get edgelist
edgelist = get.edgelist(mis_graph)


# ========================================================
# Plot
# default: plot(mis_graph)
# ========================================================
# ARC DIAGRAM


# edges
edges = edgelist
# how many edges
ne = nrow(edges)
# get nodes
nodes = unique(as.vector(edges))
nums = seq_along(nodes)
# how many nodes
nn = length(nodes)  
# ennumerate
nodes = names[ord]
nums = 1:length(nodes)
# node labels coordinates
nf = rep(1 / nn, nn)
# node labels center coordinates
fin = cumsum(nf)
ini = c(0, cumsum(nf)[-nn])
centers = (ini + fin) / 2
names(centers) = nodes

# arcs coordinates
# matrix with numeric indices
e_num = matrix(0, nrow(edges), ncol(edges))
for (i in 1:nrow(edges))
{
  e_num[i,1] = centers[which(nodes == edges[i,1])]
  e_num[i,2] = centers[which(nodes == edges[i,2])]
}
# max arc radius
radios = abs(e_num[,1] - e_num[,2]) / 2
max_radios = which(radios == max(radios))
max_rad = unique(radios[max_radios] / 2)
# arc locations
locs = rowSums(e_num) / 2


# prepare to save image
setwd("/Users/gaston/Documents/Gaston/GotPlot/images")
tiff("arcdiagram_miserables.tiff", width=1000, height=450)
# set graphic margins
op = par(mar=c(8,0.5,3,0.5), bg = "white")
plot.new()
plot.window(xlim=c(-0.025, 1.025), ylim=c(-0.01, 1*max_rad*2))
# plot connecting arcs
z = seq(0, pi, l=100)
for (i in 1:nrow(edges))
{
  radio = radios[i]
  x = locs[i] + radio * cos(z)
  y = radio * sin(z)
  lines(x, y, col=hsv(h=0, s=0, v=0.4, alpha=0.4), 
        lwd=1.1*ewidths[i], lend=1, ljoin=2, lmitre=1)
}
# add circles
points(x=centers, y=rep(0,nn), pch=21, col=borders[groups[ord]+1], 
       bg=vpal[groups[ord]+1], cex=vcex[ord])
# add node names
mtext(nodes, side=1, line=-1, at=centers, cex=1, 
      col="gray30", las=2)
par(op)
dev.off()


# ====================================================================
# source code of function arcDiagram
#source(file="/Users/gaston/Documents/Gaston/GoogleSite/StarWars/arcDiagram.R")
#arcDiagram(edgelist, sorted=FALSE, col=hsv(h=0, s=0, v=0.6, alpha=0.4),
#           lwd=ewidths, col.nodes = ncols, mar=c(8,1,3,1))
