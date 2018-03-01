setwd('/Users/jyoshimi/ipython/scientometrics')
# See http://htmlpreview.github.io/?https://github.com/massimoaria/bibliometrix/master/vignettes/bibliometrix-vignette.html


require('bibliometrix')

# files <- readFiles('citationRecord.txt')
files <- readFiles('citations2.txt','citations3.txt')

df <- convert2df(files,dbsource = "isi", format = "plaintext")
View(df)

x <- metaTagExtraction(M = df, Field = "CR_AU")
unlist(strsplit(x$CR_AU[1], ";"))
y <- cocMatrix(x, Field = "CR_AU", type = "matrix", sep = ";")
z <- strsplit(y$author, ';')

for(i in 1:length(y$author)){
  y$author[i] = z[[i]][1]
  }

y <- as.data.frame(y)
y$author <- x$AU

yPrime = aggregate(. ~ author, FUN = sum, data = y)

write.csv(y, file = "test matrix")


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

