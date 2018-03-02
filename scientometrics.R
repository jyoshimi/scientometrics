setwd('/Users/jyoshimi/ipython/scientometrics')
# See http://htmlpreview.github.io/?https://github.com/massimoaria/bibliometrix/master/vignettes/bibliometrix-vignette.html
setwd('GitHub/scientometrics/')

require('bibliometrix')
require('stringr')

# files <- readFiles('citationRecord.txt')
files <- readFiles('data/citations1-500.txt','data/citations501-1000.txt', 'data/citations1001-1500.txt', 'data/citations1501-2000.txt')

df <- convert2df(files,dbsource = "isi", format = "plaintext")
x <- metaTagExtraction(M = df, Field = "CR_AU")
y <- cocMatrix(x, Field = "CR_AU", type = "matrix", sep = ";")
z <- strsplit(x$AU, ';')

authors <- c()

for(i in 1:length(z)){
  authors <- c(authors, z[[i]][1])
  }

y <- as.data.frame(y)
y$author <- authors


yOrdered <- y[order(row.names(y)),]
yOrdered <- yOrdered[, order(colnames(yOrdered))]

#clean the rows#
yOrdered$author <- str_replace(string = yOrdered$author, replacement = "\\1", pattern = "^([A-Z]+[[:space:]][A-Z]{1})([[:space:]])([A-Z]{1})") # removes all second initials (marion j l -> marion j)
yOrdered$author <- str_replace(string = yOrdered$author, pattern = "(^[A-Z]+[[:space:]][A-Z]{1})([A-Z]+)", replacement = "\\1") # remove all full first names (marion jeanluc -> marion j)

yPrime <- aggregate(. ~ author, FUN = sum, data = yOrdered)
row.names(yPrime) <- yPrime$author
yPrime <- yPrime[, which(colnames(yPrime) != "author")]

#clean up columns now#

yPrimeReversed <- as.data.frame(t(yPrime))
yPrimeReversed$authors <- row.names(yPrimeReversed)
yPrimeReversed$authors <- str_replace(string = yPrimeReversed$authors, pattern = "ARIST[A-Z]*", replacement = "ARISTOTLE")
yPrimeReversed$authors <- str_replace_all(string = yPrimeReversed$authors, pattern = "HUSS*[EA]R?L?[^YS]([[:space:]]\\w*)*", replacement = "HUSSERL E")
yPrimeReversed$authors <- str_replace(string = yPrimeReversed$authors, pattern = "(^[A-Z]+[[:space:]][A-Z]{1})([A-Z]+)", replacement = "\\1") # remove all full first names (marion jeanluc -> marion j)
yPrimeReversed$authors <- str_replace(string = yPrimeReversed$authors, replacement = "\\1", pattern = "(^[A-Z]+[[:space:]][A-Z]{1})([[:space:]])([A-Z]{1})([[:space:]]*[A-Z]*)") # removes all second initials (marion j l -> marion j)
yPrimeReversedPrime <- aggregate(. ~ authors, FUN = sum, data = yPrimeReversed)
row.names(yPrimeReversedPrime) <- yPrimeReversedPrime$authors
yPrimeReversedPrime <- yPrimeReversedPrime[, which(colnames(yPrimeReversedPrime) != "authors")]

#finally!#

finalY <- t(yPrimeReversedPrime)
smallerFinalY <- finalY[, which(colSums(finalY) > 5, arr.ind = T)]
smallerFinalY <- smallerFinalY[which(rowSums(smallerFinalY) > 5, arr.ind =  T),]

write.csv(finalyY, file = "cleanish complete adjancency matrix.csv", row.names = T, col.names = T)
write.csv(smallerFinalY, file = "cleanish small adjacency matrix.csv", row.names = T, col.names = T) # includes only authors with more than 5 citations from authors with total citations to other authors larger than 5.

results <- biblioAnalysis(df,sep=";")
View(results)

summary(object=results,k=10, pause=FALSE)

plot(x=results,k=10, pause=FALSE)

cr <- citations(df,field="article",sep=".  ")
cr$Cited[1:10]

cr <- citations(df,field="author",sep=".  ")
cr$Cited[1:20]

NetMatrix <- biblioNetwork(df, analysis = "co-citation", network = "references", sep=".  ")
net=networkPlot(NetMatrix, normalize = "salton", weighted=T, n = 20, Title = "Authors' Coupling", type = "fruchterman", size=FALSE,remove.multiple=TRUE)

net=networkPlot(NetMatrix, n = 15, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE)

NetMatrix <- biblioNetwork(df, analysis = "co-occurrences", network = "keywords", sep = ";")
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 20, Title = "Keyword Co-occurrences", type = "fruchterman", size=T)

CS <- conceptualStructure(df,field="ID", minDegree=4, k.max=5, stemming=FALSE, labelsize=10)

histResults <- histNetwork(df, n = 20, sep = ".  ")
net <- histPlot(histResults, size = FALSE,label=TRUE, arrowsize = 0.5)

