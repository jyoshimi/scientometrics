setwd('/Users/jyoshimi/ipython/scientometrics/v2')
# See http://htmlpreview.github.io/?https://github.com/massimoaria/bibliometrix/master/vignettes/bibliometrix-vignette.html
setwd('GitHub/scientometrics/v2/')
require('bibliometrix')
require('stringr')
require('reshape2')
require('igraph')
require('ggraph')
files <- readFiles('savedref_8.txt', 'savedref_7.txt', 'savedref_6.txt', 
                   'savedref_5.txt', 'savedref_4.txt', 'savedref_3.txt', 
                   'savedref_2.txt', 'savedref_1.txt')

# Convert files to dataframe. isi is web of science
df <- convert2df(files,dbsource = "isi", format = "plaintext")

# Create a new data frame, that is exactly like DF but includes a new 
# column with a new "authors" meta-tag (a meta-tag is a combination of basic tags)
# All this does is take the CR tag, and strip out the authors (open a raw file to grok further)
df <- metaTagExtraction(M = df, Field = "CR_AU")

# Remove rows which don't have cross-reffed authors
df = df[which(df$CR_AU != "NA"),]

# Create the co-occurrence matrix.  Each row is an author of the downloaded docs,
# Each column is an author or authors (for multi-authored works) that the original author cites.
# Note that the same cited author can occur in multiple columns
# NOTE: Don't view it from R
cocm <- cocMatrix(df, Field = "CR_AU", type = "matrix", sep = ";")

# Save the matrix in case I want to do python stuff on it
#write.csv(cocm,file = "cocm.csv",sep = ",")

# Take all authors from original data frame.  Semicolon separates multiple authors.
# We are currently ignoring second authors because too difficult.
z <- strsplit(df$AU,";")
authors <- c()
for(i in 1:length(z)){
  authors <- c(authors, z[[i]][1])
  }

# Convert cocm matrix to a dataframe
cocm <- as.data.frame(cocm)

# Add an extra column with the first author of a document
cocm$author <- authors

# Alphabetically orders documents by row and by colums.  Rows and columns alphabetically ordered
cocmOrdered <- cocm[order(row.names(cocm)),]
cocmOrdered <- cocmOrdered[, order(colnames(cocmOrdered))]

# Clean up authors
# print(authors)
# removes all second initials (marion j l -> marion j)
cocmOrdered$author <- str_replace(string = cocmOrdered$author, replacement = "\\1", pattern = "^([A-Z]+[[:space:]][A-Z]{1})([[:space:]])([A-Z]{1})") 
# remove all full first names (marion jeanluc -> marion j)
cocmOrdered$author <- str_replace(string = cocmOrdered$author, pattern = "(^[A-Z]+[[:space:]][A-Z]{1})([A-Z]+)", replacement = "\\1") 

# Merge all rows that share an author
cocmPrime <- aggregate(. ~ author, FUN = sum, data = cocmOrdered)
# Now that author is unnique it can be used for row names
row.names(cocmPrime) <- cocmPrime$author
# Removes now unneeded author column
cocmPrime <- cocmPrime[, which(colnames(cocmPrime) != "author")]

# Do the same kind of thing for columns. Merge columns with same author.
# Method here was to transpose and use the method above
#clean up columns now TODO: Change names to cocm stuff
yPrimeReversed <- as.data.frame(t(cocmPrime)) # transpose
# Add a new column with row-names
yPrimeReversed$authors <- row.names(yPrimeReversed)
# Regex away on the columns this time
yPrimeReversed$authors <- str_replace(string = yPrimeReversed$authors, pattern = "ARIST[A-Z]*", replacement = "ARISTOTLE")
yPrimeReversed$authors <- str_replace_all(string = yPrimeReversed$authors, pattern = "HUSS*[EA]R?L?[^YS]([[:space:]]\\w*)*", replacement = "HUSSERL E")
yPrimeReversed$authors <- str_replace(string = yPrimeReversed$authors, pattern = "(^[A-Z]+[[:space:]][A-Z]{1})([A-Z]+)", replacement = "\\1") # remove all full first names (marion jeanluc -> marion j)
# removes all second initials (marion j l -> marion j)
yPrimeReversed$authors <- str_replace(string = yPrimeReversed$authors, replacement = "\\1", pattern = "(^[A-Z]+[[:space:]][A-Z]{1})([[:space:]])([A-Z]{1})([[:space:]]*[A-Z]*)") 

# This is where the merge happens
yPrimeReversedPrime <- aggregate(. ~ authors, FUN = sum, data = yPrimeReversed)
row.names(yPrimeReversedPrime) <- yPrimeReversedPrime$authors
yPrimeReversedPrime <- yPrimeReversedPrime[, which(colnames(yPrimeReversedPrime) != "authors")]

# Transpose back
finalY <- t(yPrimeReversedPrime)

# Get rid of authors cited less than 5 times and who cited someone less than five times
# (includes only authors with more than 5 citations from authors with total citations to other authors larger than 5.)
smallerFinalY <- finalY[, which(colSums(finalY) > 5, arr.ind = T)]
smallerFinalY <- smallerFinalY[which(rowSums(smallerFinalY) > 5, arr.ind =  T),]

  #PABLO: WE CAN GENERATE AN EDGE LIST WITH THE MELT FUNCTION

edgeList <- melt(finalY)
colnames(edgeList) <- c("Source", "Target", "Weight")
edgeList <- edgeList[which(edgeList$Weight > 0),]
edgeList <- edgeList[which(!is.na(edgeList$Source)),]
edgeList$Source <- as.character(edgeList$Source)
edgeList$Target <- as.character(edgeList$Target)
edgeList$Weight <- as.numeric(edgeList$Weight)
edgeList <- as.matrix(edgeList)
write.csv(x = edgeList, file = "smallEdgeList.csv", row.names = F)
graphWithEdges <- graph.data.frame(edgeList, directed = T)

ggraph(graphWithEdges) + 
  geom_edge_link() + 
  geom_node_point()








# Write the final matrix
write.csv(smallerFinalY, file = "cleaned_matrix.csv", row.names = T, col.names = T)

# Stuff from before

# results <- biblioAnalysis(df,sep=";")
# View(results)
# 
# summary(object=results,k=10, pause=FALSE)
# 
# plot(x=results,k=10, pause=FALSE)
# 
# cr <- citations(df,field="article",sep=".  ")
# cr$Cited[1:10]
# 
# cr <- citations(df,field="author",sep=".  ")
# cr$Cited[1:20]
# 
# NetMatrix <- biblioNetwork(df, analysis = "co-citation", network = "references", sep=".  ")
# net=networkPlot(NetMatrix, normalize = "salton", weighted=T, n = 20, Title = "Authors' Coupling", type = "fruchterman", size=FALSE,remove.multiple=TRUE)
# 
# net=networkPlot(NetMatrix, n = 15, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE)
# 
# NetMatrix <- biblioNetwork(df, analysis = "co-occurrences", network = "keywords", sep = ";")
# net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 20, Title = "Keyword Co-occurrences", type = "fruchterman", size=T)
# 
# CS <- conceptualStructure(df,field="ID", minDegree=4, k.max=5, stemming=FALSE, labelsize=10)
# 
# histResults <- histNetwork(df, n = 20, sep = ".  ")
# net <- histPlot(histResults, size = FALSE,label=TRUE, arrowsize = 0.5)

