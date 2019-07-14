
# load packages
library(XML)


# REI url 
rei_url = "http://www.rei.com"

# search tent hrefs
search.hrefs = c("/search?cat=4500029&cat=4500457&search=tents&seq=1&page_size=109&hist=query%2Ctents^cat%2C4500029%3ATents^cat%2C4500457%3ABackpacking+Tents&pageSizeRedirect",   "/search?cat=4500029&cat=40007159&search=tents&scv_page_size=109&seq=1&hist=query%2Ctents^cat%2C4500029%3ATents^cat%2C40007159%3AFamily+%26+Car+Camping+Tents")

# get tent hrefs
tent.hrefs.list = as.list(1:length(search.hrefs))
for (i in 1:length(search.hrefs))
{
   # parse html
   search_url = paste(rei_url, search.hrefs[i], sep="")
   doc.rei = htmlTreeParse(search_url, useInternalNodes=TRUE)
   # get root node
   top.rei = xmlRoot(doc.rei)
   # get item hrefs
   tent.hrefs.list[[i]] = xpathSApply(top.rei, "//ul[@class='productBox']/li/a[@href]", xmlAttrs)
}
# vector of tent hrefs
tent.hrefs = unlist(tent.hrefs.list)

# get tent names
tent.pat = "/product/[0-9]{6}/(.*)"
grep.tent = grep(tent.pat, tent.hrefs, value=TRUE)
tent.names = gsub(tent.pat, "\\1", grep.tent)

# how many tents
n.tents = length(tent.hrefs)

tent.list = as.list(1:n.tents)
prices = rep(1:n.tents)
for (i in 1:n.tents)
{
   tent_url = paste(rei_url, tent.hrefs[i], sep="")
   doc.tent = htmlTreeParse(tent_url, useInternalNodes=TRUE)
   top.tent = xmlRoot(doc.tent)
   # get table of specifications
   x = readHTMLTable(doc.tent)
   tent.list[[i]] = as.matrix(cbind(x[[1]][,1], x[[3]]))
   # get price
   priz = xpathSApply(top.tent, "//ul[@class='itemprice cf']/li", xmlValue)
   if (length(priz)==1) {
       prices[i] = priz
   } else {
       prices[i] = priz[2]
   }
}
names(tent.list) = tent.names

# cleaning the prices
# remove ' ', '$', and '*' symbols from prices
newprices = sub("[ \\$]{1,}", "", prices)
newprices = sub("\\*", "", newprices)

# transforming data into a dataframe structure
specs = as.character(tent.list[[1]][,1])
spec.list = as.list(1:23)
for (k in 1:23)
{
   aux = lapply(tent.list, function(x) x[which(x[,1]==specs[k]),2])
   spec.aux = rep(NA, n.tents)
   for (i in 1:n.tents)
   {
      if (length(aux[[i]])!=0)
          spec.aux[i] = aux[[i]]
   }
   spec.list[[k]] = spec.aux
}

TENT = cbind(tent.names, as.numeric(newprices))
for (k in 1:23)
    TENT = cbind(TENT, cbind(unlist(spec.list[[k]])))
dim(TENT)

newspecs = gsub('[[:punct:]]', " ", specs)
newspecs = gsub('[ \t]{2,}', " ", newspecs)
newspecs = gsub(" ", ".", newspecs)

colnames(TENT) = c("Name", "Price", newspecs)
rownames(TENT) = 1:n.tents

# remove last three items (useless)
TENT = TENT[,1:92]

setwd("D:/GoogleSites/ReiTents")
# write.csv(TENT, "TENT.csv")
TENT = read.csv("TENT.csv", row.names=1, stringsAsFactors=FALSE)


# ********************************************
writeLines(TENT$Name, "TentNames.txt")
xnames = readLines("ReiTentNames.txt")
xbrands = readLines("ReiTentBrands.txt")

NEWTENT = TENT
NEWTENT$Model = xnames
NEWTENT$Brand = xbrands
NEWTENT = NEWTENT[,c(1,26,27,2:25)]
write.csv(NEWTENT, "NEWTENT.csv")
# ********************************************
# ======================================================

setwd("D:/GoogleSites/ReiTents")
TENT = read.csv("NEWTENT.csv", row.names=1, header=TRUE, stringsAsFactors=FALSE)

# clean Average weight metric
clean.weight = function(x)
{
    wpat = "([0-9]+\\.[0-9]{1,2}).*"
    wgrep = grep(wpat, x, value=TRUE)
    wres = sub(wpat, "\\1", wgrep)
    if (length(wres)==0)
        wres = NA
    wres
}
Weight.ave = sapply(TENT$Average.weight.metric, clean.weight)


# floor dimensions
TENT$Floor.dimensions.metric[70] = "254 x 218 centimeters"
TENT$Floor.dimensions.metric[86] = "300 x 230 centimeters"

# clean floor dimension 1
clean.dim1 = function(x)
{
  fpat1 = "([0-9]{3}).*"
  fgrep1 = grep(fpat1, x, value=TRUE)
  fres1 = sub(fpat1, "\\1", fgrep1)
  if (length(fres1)==0)
      fres1 = NA
  fres1
}
Floor.dim1 = sapply(TENT$Floor.dimensions.metric, clean.dim1)

# clean floor dimension 2
clean.dim2 = function(x)
{
  fpat2 = "[0-9]{3} x ([0-9]{2,3}).*"
  fgrep2 = grep(fpat2, x, value=TRUE)
  fres2 = sub(fpat2, "\\1", fgrep2)
  if (length(fres2)==0)
      fres2 = NA
  fres2
}
Floor.dim2 = sapply(TENT$Floor.dimensions.metric, clean.dim2)

# pick height
TENT$Peak.height.metric[75] = "183 centimeters"
TENT$Peak.height.metric[76] = "168 centimeters"
TENT$Peak.height.metric[86] = "190 centimeters" 

# clean pick height
clean.height = function(x)
{
  hpat = "([0-9]{2,3}).*"
  hgrep = grep(hpat, x, value=TRUE)
  hres = sub(hpat, "\\1", hgrep)
  if (length(hres)==0)
      hres = NA
  hres
}
Height = sapply(TENT$Peak.height.metric, clean.height )

# clean weight
Weight = TENT$Weight.g.
Weight = sub(" grams", "", Weight)

# select variables
select.vars = c("Name","Model","Brand","Price","Best.use","Seasons","Design.type",
    "Sleeping.capacity","Number.of.doors", "Canopy.fabric", "Floor.fabric",
    "Rainfly.fabric", "Number.of.poles", "Pole.material", "Pole.diameter",
    "Packed.size")
tents = TENT[,select.vars]

# define data with quantitative dimensions
dims = cbind(Weight.ave=as.numeric(Weight.ave), Weight=as.numeric(Weight), 
   Floor.dim1=as.numeric(Floor.dim1), Floor.dim2=as.numeric(Floor.dim2), 
   Height=as.numeric(Height))

which(tents$Design.type=="Freestanding  tent")

# bind tenst and dims
X = cbind(tents, dims)
# write.csv(X, "ReiTents.csv")



# ============================== data cleaning ==============================
tents = read.csv("ReiTents.csv", row.names=1, stringsAsFactors=FALSE)

nt = nrow(tents)
canopy = tolower(tents$Canopy.fabric)
canopy.split = strsplit(canopy, "/")
canopy.fab = rep("", nt)
mesh = rep("NO", nt)
mesh.type = rep("none", nt)
for (i in 1:nt)
{
    grep.mesh = grep("mesh", canopy.split[[i]])   
    if (length(grep.mesh) == 0)
        canopy.fab[i] = canopy.split[[i]]
    if (length(grep.mesh) != 0)
    {
        mesh[i] = "YES"        
        mesh.type[i] = canopy.split[[i]][grep.mesh]
        if (grep.mesh == 1)
            canopy.fab[i] = canopy.split[[i]][2]
        if (grep.mesh == 2)
            canopy.fab[i] = canopy.split[[i]][1]
    }
}

# remove space
mesh.type = sub("^ ", "", mesh.type)

# remove space at the end
canopy.fab = sub("$ ", "", canopy.fab)
# remove punctuation
canopy.fab = sub("[[:punct:]]", " ", canopy.fab)

# coated fabric
coated = rep("NO", nt)
coated[grep("coated", canopy.fab)] = "YES"

# nylon or polyester canopy fabric
canopy.nylpol = rep("other", nt)
canopy.nylpol[grep("nylon", canopy.fab)] = "nylon"
canopy.nylpol[grep("polyester", canopy.fab)] = "polyester"

# ripstop or taffeta canopy fabric
canopy.riptaf = rep("none", nt)
canopy.riptaf[grep("ripstop", canopy.fab)] = "ripstop"
canopy.riptaf[grep("taffeta", canopy.fab)] = "taffeta"

# add variables
tents$Canopy.coated = coated
tents$Canopy.material = canopy.nylpol
tents$Canopy.weave = canopy.riptaf
tents$Canopy.mesh = mesh
tents$Canpy.mesh.type = mesh.type
tents$Rainfly.fabric[is.na(tents$Rainfly.fabric)] = "Not applicable"
tents$Floor.area = (tents$Floor.dim1/100) * (tents$Floor.dim2/100)

poles = tolower(tents$Pole.material)
# remove punctuation and spaces
poles = gsub("[[:punct:]]", " ", poles)
poles = gsub('[ \t]{2,}', " ", poles)
poles = gsub("^ ", "", poles)

poles = gsub("alumiinum", "aluminum", poles)
poles = gsub("aluminium", "aluminum", poles)
poles = gsub("aluminuim", "aluminum", poles)

poles.material = rep("other", nt)
poles.material[grep("aluminum", poles)] = "aluminum"
poles.material[grep("carbon", poles)] = "carbon"

poles.type = rep("other", nt)
poles.type[grep("featherlight", poles)] = "featherlite"
poles.type[grep("featherlite", poles)] = "featherlite"
poles.type[grep("pressfit", poles)] = "pressfit"
poles.type[grep("fit", poles)] = "pressfit"
poles.type[grep("carbon", poles)] = "carbon"
cbind(poles, poles.type)

tents$Pole.alucar = poles.material
tents$Pole.type = poles.type

# rearranging columns
tents = tents[,c(1:19,29:30,20:27)]
write.csv(tents, "ReiTents.csv")


# ============================== data analysis ==============================
library(ggplot2)
library(rpart)
library(FactoMineR)

tents <- read.csv("data/tents.csv", row.names=1, stringsAsFactors=FALSE)


quali <- c("brand", "bestuse", "seasons", "design", 
           "capacity", "canopy.material")

summary(tents[,quali])
mca <- MCA(tents[,quali])




persons = substr(tents$Sleeping.capacity, 1, 1)
tents$Persons = as.numeric(persons)

# histogram of prices
ggplot(tents, aes(x=Price)) +
geom_histogram(binwidth=50, fill="steelblue", colour="white")

# brands and sleeping capacity
ggplot(tents, aes(x=Brand, y=Price, color=Brand)) + 
geom_jitter(position=position_jitter(width=.1), size=3, alpha=.65) + 
facet_wrap(~ Sleeping.capacity) +
scale_colour_manual(values=brewer.pal(8, "Dark2")) +
opts(axis.text.x = theme_text(angle=90))

# brands and sleeping capacity, and best use
ggplot(tents, aes(x=Brand, y=Price, color=Brand, shape=Best.use)) + 
geom_jitter(position=position_jitter(width=.1), size=3, alpha=.65) + 
facet_wrap(~ Sleeping.capacity) +
scale_colour_manual(values=brewer.pal(8, "Dark2")) +
opts(axis.text.x = theme_text(angle=90))

# scatter diagram Area -vs- Price by best use
ggplot(data=tents, aes(x=Floor.area, y=Price, colour=Best.use)) + 
geom_point(size=2.5) 

# scatter diagram Area -vs- Price by best use, with regression line
ggplot(tents, aes(Floor.area, Price, color=Best.use)) + 
geom_point(size=2.5) + 
geom_smooth(aes(group=Best.use), method="lm") 

# scatter diagram Area -vs- Price by best use and canopy material
ggplot(data=tents, aes(x=Floor.area, y=Price, shape=Best.use, colour=Canopy.material)) + 
geom_point(size=2.5) 


# scatter diagram Weight -vs- Price by best use
ggplot(data=tents, aes(x=Weight, y=Price, colour=Best.use)) + 
geom_point(size=2.5) 

# scatter diagram Weight -vs- Price by best use, with regression line
ggplot(tents, aes(Weight, Price, color=Best.use)) + 
geom_point(size=2.5) + 
geom_smooth(aes(group=Best.use), method="lm") 


ggplot(data=tents, aes(x=Weight, y=Price, colour=Brand)) + 
geom_point(size=2.5) +
scale_colour_manual(values=brewer.pal(8, "Dark2")) 

ggplot(tents, aes(Weight, Price, label=Model)) + 
geom_text(size=3)

ggplot(tents, aes(Weight, Price, label=Model)) + 
geom_text(size=3) + facet_wrap(~ Best.use)


ggplot(tents, aes(Weight, Price, label=Model, color=Canopy.material)) + 
geom_text(size=3)

ggplot(tents, aes(Weight, Price, label=Model, color=Canopy.mesh)) + 
geom_text(size=3)

ggplot(tents, aes(Floor.area, Price, label=Model, color=Canopy.mesh)) + 
geom_text(aes(size=Weight))

# plots Weight -vs- Floor Area
ggplot(tents, aes(Weight, Floor.area)) +
geom_point(aes(size=Price))

ggplot(tents, aes(Weight, Floor.area, color=Brand)) +
geom_point(aes(size=Price)) +
scale_colour_manual(values=brewer.pal(8, "Dark2"))


# first decision tree
fit1 = rpart(Price ~ Brand + Best.use + Seasons + Design.type + Canopy.coated + 
   Canopy.material + Canopy.weave + Weight + Floor.area + Pole.alucar + Pole.type, 
   data=tents, method="anova")
plot(fit1, margin=.1)
text(fit1, use.n=TRUE, pretty=1)

# principal components with function princomop
pca1 = princomp(tents[,c("Weight","Floor.dim1","Floor.dim2","Floor.area","Height","Persons")],
   cor = TRUE)
tents$PC1 = pca1$scores[,1]
# principal components with function PCA (FactoMineR)
pca1 = PCA(tents[,c("Weight","Floor.dim1","Floor.dim2","Floor.area","Height","Persons")])
tents$PC1 = pca1$ind$coord[,1]

ggplot(tents, aes(x=PC1, y=Price, color=Brand, shape=Best.use)) + 
scale_colour_manual(values=brewer.pal(8, "Dark2")) +
geom_vline(xintercept = c(-1.09, -0.46, 2.22)) + 
geom_hline(yintercept = c(284, 393, 446)) + 
geom_point() 

ggplot(tents, aes(PC1, Price, color=Best.use)) + 
geom_point() + 
geom_smooth(aes(group=Best.use), method="lm") 


# second decicion tree
tree.fit = rpart(Price ~ Brand + Best.use + Seasons + Design.type + Canopy.coated + 
   Canopy.material + Canopy.weave + Pole.alucar + Pole.type + PC1, 
   data=tents, method="anova")
dev.new()
plot(tree.fit, margin=.05)
text(tree.fit, use.n=TRUE, pretty=1)

ggplot(tents, aes(PC1, Price, label=Model)) +
geom_text(size=3)



node1 = which(tents$PC1<c(-1.18) & tents$Brand %in% c("kelty","rei"))
node2 = which(tents$PC1<c(-1.18) & tents$Brand %in% c("big-agnes","marmot",
   "msr","nemo","sierra-designs","the-north-face") 
   & tents$Canopy.material %in% c("other","polyester"))
node3 = which(tents$PC1<c(-1.18) & tents$Brand %in% c("big-agnes","marmot",
   "msr","nemo","sierra-designs","the-north-face") 
   & tents$Canopy.material %in% c("nylon"))
node4 = which(tents$PC1>=c(-1.18) & tents$Brand %in% c("kelty","marmot",
   "rei","sierra-designs") & tents$PC1<3.305 
   & tents$Canopy.material %in% "polyester")
node5 = which(tents$PC1>=c(-1.18) & tents$Brand %in% c("kelty","marmot",
   "rei","sierra-designs") & tents$PC1<3.305 
   & tents$Canopy.material %in% c("other","nylon"))
node6 = which(tents$PC1>=c(-1.18) & tents$Brand %in% c("kelty","marmot",
   "rei","sierra-designs") & tents$PC1>=3.305) 
node7 = which(tents$PC1>c(-1.18) & tents$Brand %in% c("msr","the-north-face",
    "nemo","big-agnes") & tents$PC1<2.173)
node8 = which(tents$PC1>c(-1.18) & tents$Brand %in% c("msr","the-north-face",
    "nemo","big-agnes") & tents$PC1>=2.173)

ncols = brewer.pal(8, "Dark2")
with(tents, plot(PC1, Price, type="n"))
abline(v = c(-1.18, 2.17, 3.305), col="grey")
points(tents$PC1[node1], tents$Price[node1], col=ncols[1], pch=19)
points(tents$PC1[node2], tents$Price[node2], col=ncols[2], pch=19)
points(tents$PC1[node3], tents$Price[node3], col=ncols[3], pch=19)
points(tents$PC1[node4], tents$Price[node4], col=ncols[4], pch=19)
points(tents$PC1[node5], tents$Price[node5], col=ncols[5], pch=19)
points(tents$PC1[node6], tents$Price[node6], col=ncols[6], pch=19)
points(tents$PC1[node7], tents$Price[node7], col=ncols[7], pch=19)
points(tents$PC1[node8], tents$Price[node8], col=ncols[8], pch=19)


