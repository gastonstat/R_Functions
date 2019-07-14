
#install.packages(c("tm", "wordcloud"))

# load required packages
library(XML)
library(tm)
library(wordcloud)
library(RColorBrewer)

# define base pubmed eutils url
eutils_url = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils"

# search "psoriatic arthritis"
eutils_search = paste(eutils_url, 
	"/esearch.fcgi?db=pubmed&term=psoriatic+arthritis[title]&retmax=2200", sep="")

# parse eutils_search
docId = xmlTreeParse(eutils_search, useInternalNodes=TRUE)

# get the top level xml node
topId = xmlRoot(docId)

# extract Ids
Idlist = xpathSApply(topId, "//Id", xmlValue)

#=====================================================================
# Get the xml info of the articles and extract desired information
#=====================================================================

# define the eutils summary url
eutils_summary = "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id="

# add the list of Ids to the eutils summary url
Idsummary = paste(eutils_summary, paste(Idlist[1:800], collapse=","), sep="")

# parse Idsummary
doc = xmlTreeParse(Idsummary, isURL=TRUE, useInternal=TRUE)
doc = xmlTreeParse(Idsummary, useInternalNodes=TRUE)


# get the top level xml node
top = xmlRoot(doc)

# extract titles and language
titles = xpathSApply(top, "//Item[@Name='Title']", xmlValue)
langs = xpathSApply(top, "//Item[@Name='Lang']", xmlValue)

# select publications in english
eng = which(langs == "English")

# convert to lower case
titles.eng = tolower(titles[eng])

# remove punctuation symbols
titles.eng = gsub('[[:punct:]]', " ", titles.eng)

# remove numbers
titles.eng = gsub('[[:digit:]]', " ", titles.eng)

# remove tabs and more than one space
titles.eng = gsub('[ \t]{2}', " ", titles.eng)

# split titles by space
titles.split = strsplit(titles.eng, "[[:space:]]")

# remove stopwords
rm.stopwords = function(x)
{
	nop = x %in% c(stopwords(), "psoriatic", "psoriasis", 
	"arthritis", "using", "psa", "annual", "meeting", "society",
	"omeract", "grappa")
	x[!nop]
}
titles.split = lapply(titles.split, rm.stopwords)

# joining everything back together
titles.clean = sapply(titles.split, function(x) paste(x, collapse=" "))


# ======================================================================
# text cleaning (joining similar words)
# ======================================================================

# corpus
titles.corpus = Corpus(VectorSource(titles.clean))

# create TermDocumentMatrix
tdm = TermDocumentMatrix(titles.corpus)

# remove same sparse terms
tdm = removeSparseTerms(tdm, .99)
m = as.matrix(tdm)

# rearrange
v = sort(rowSums(m), decreasing=TRUE)

# create data frame of words and frequencies
d = data.frame(word=names(v), freq=v)

# define color palette Blues
pal = brewer.pal(9, "Blues")[-c(1:3)]

# First plot attempt of the word cloud
wordcloud(d$word, d$freq, colors=pal, random.order=FALSE)

# ======================================================================
# text cleaning (joining similar words)
# ======================================================================

# inspecting words (sorted alphabetically)
sort(d$word)

# joining everything back together
titles.clean = sapply(titles.split, function(x) paste(x, collapse=" "))

# create vector of repeated words
rep.words = c("patients", "genetics", "factors", "therapies",
"association", "biological", "effects", "effectiveness", "joint",
"jointss", "reported", "rheumatoid", "treated", "tumour")

# create vector of substituting words
uni.words = c("patient", "genetic", "factor", "therapy", 
"associated", "biologic", "effect", "efficacy", "joints", 
"joints", "report", "rheumatology", "treatment", "tumor")

# substituting similar words
for (i in 1:length(rep.words))
	titles.clean = gsub(rep.words[i], uni.words[i], titles.clean)
	
# corpus
titles.corpus = Corpus(VectorSource(titles.clean))

# create TermDocumentMatrix
tdm = TermDocumentMatrix(titles.corpus)

# remove same sparse terms
tdm = removeSparseTerms(tdm, .99)
m = as.matrix(tdm)

# rearrange
v = sort(rowSums(m), decreasing=TRUE)

# create data frame of words and frequencies
d = data.frame(word=names(v), freq=v)

# define color palette Blues
pal = brewer.pal(9, "Blues")[-c(1:3)]

# First plot attempt of the word cloud
wordcloud(d$word, d$freq, colors=pal, random.order=FALSE)


# =============================================================

library(ca)

elim = which(colSums(m) == 0)
m = m[,-elim]

M = m %*% t(m)

diag(M) = 0

my.ca = ca(M)

x = my.ca$rowcoord[,1]
y = my.ca$rowcoord[,2]

pal = colorRampPalette(c("gray50", "white", "red"))
mycols = pal(100)
colindex = round(rowSums(m) / max(rowSums(m)) * length(mycols))

ord = order(rowSums(m))

par(mar=c(2,2,3,2))
plot(x, y, type="n", bty='n', xaxt='n', yaxt='n', xlab='', ylab='',
	xlim = 1.05 * c(min(x), max(x)), ylim=1.05 * c(min(y), max(y)))
mtext(text = "Correspondence Analysis Map", side=3, cex=1, line=0.5)
rect(-7, -5, 5, 5, col="gray20")
box(col="gray90")
text(x[ord], y[ord], labels=my.ca$rownames[ord], 
    cex=log10(rowSums(m)[ord]/2), 
	col=mycols[colindex][ord])

text(x[ord], y[ord], labels=my.ca$rownames[ord], 
    cex=log10(sqrt(rowSums(m)[ord])), 
	col=mycols[colindex][ord])
	

