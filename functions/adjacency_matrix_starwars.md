
# set your working directory (don't use mine!)
# setwd("/Users/gaston/Documents/Gaston/GoogleSite/StarWars")

# load package plyr
require(plyr)
require(tm)
require(RColorBrewer)

# import data tables
Ep4 = read.table("SW_EpisodeIV.txt")

# select dialogues
diags4 = Ep4[,2]

# select characters
chars4 = Ep4[,1]

# how many dialogues
n4 = length(diags4)

# =======================================================
# Find most talkative characters
# =======================================================
# Keep in mind that Artoo and Chewie are not included 
# since they don't really have dialogues in english

# get absolute frequencies
char4_names = table(chars4)

# get quantity number of names
y = as.vector(char4_names)
# sort by frequency
yord = order(y, decreasing=TRUE)

# inspect frequencies of names
head(sort(y, decreasing=TRUE))
head(names(char4_names)[yord], 20)
tail(names(char4_names)[yord], 20)

# barplot with the top 50 characters (most talkative)
par(mar = c(7, 4, 4, 1))
top_chars4 = head(char4_names[yord], 50)
barplot(top_chars4, las=2, cex.names=0.7, 
        border=NA, ylim=c(0,300), ylab="num of dialogues")
title(c("Star Wars (Episodes IV)", "top 50 most talkative characters"),
      cex.main=0.9)

# get top names
who_top_chars = names(top_chars4)


# top character names (sorted)
aux_top_chars = who_top_chars

# =====================================================================
# Episode IV: get most frequent terms of top characters
# =====================================================================

# identify top characters in episode IV
aux_chars4 = chars4 %in% aux_top_chars
top_chars_eps4 = unique(chars4[aux_chars4])
# empty vector to collect dialogues of top chars
diag4_top_chars = rep("", length(top_chars_eps4))
# collect dialogues for top characters
for (i in 1:length(top_chars_eps4))
{
  diag4_top_chars[i] = paste(diags4[chars4 == top_chars_eps4[i]], collapse=" ")
}
names(diag4_top_chars) = top_chars_eps4

# get corpus for top characters in episode IV
diag4_corpus = Corpus(VectorSource(diag4_top_chars))

# apply text transformatioins
diag4_corpus = tm_map(diag4_corpus, tolower)
diag4_corpus = tm_map(diag4_corpus, removeWords, 
                      c(stopwords("english"),"comlink"))
diag4_corpus = tm_map(diag4_corpus, removeNumbers)
diag4_corpus = tm_map(diag4_corpus, removePunctuation)
diag4_corpus = tm_map(diag4_corpus, stripWhitespace)

# term-document matrix
diag4_tdm = TermDocumentMatrix(diag4_corpus)
dim(diag4_tdm)
diag4_tdm = as.matrix(diag4_tdm)
# get top terms for top characters in episode IV
diag4_top_terms = as.list(1:ncol(diag4_tdm))
for (j in 1:ncol(diag4_tdm))
{
  aux = sort(diag4_tdm[,j], decreasing=TRUE)
  diag4_top_terms[[j]] = head(aux, 10)
}
names(diag4_top_terms) = colnames(diag4_tdm)

# adjacency matrix
M = t(diag4_tdm) %*% diag4_tdm
# set elements in diagonal to zero
diag(M) = 0

heatmap(M, Rowv=NA, Colv=NA, col=brewer.pal(8, "YlGnBu"))
heatmap(M, col=brewer.pal(8, "Blues"))
