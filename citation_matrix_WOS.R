# setwd('/Users/jyoshimi/ipython/scientometrics')
# See http://htmlpreview.github.io/?https://github.com/massimoaria/bibliometrix/master/vignettes/bibliometrix-vignette.html
# setwd('Documents/GitHub/phenomenology/')

require('bibliometrix')
require('stringr')
require('tidyverse')

files <- readFiles('data/WoS/citations-1-500.txt','data/WoS/citations-501-1000.txt', 
                   'data/WoS/citations-1001-1500.txt', 'data/WoS/citations-1501-2000.txt', 
                   'data/WoS/citations-2001-2500.txt', 'data/WoS/citations-2501-3000.txt',
                   'data/WoS/citations-3001-3500.txt', 'data/WoS/citations-3501-4000.txt',
                   'data/WoS/citations-4001-4500.txt','data/WoS/citations-4501-4543.txt')
raw.articles <- bibliometrix::convert2df(files,dbsource = "isi", format = "plaintext")

parsed.articles <- metaTagExtraction(M = raw.articles, Field = "CR_AU")
# extract all unique authors from references

citing.matrix <- cocMatrix(parsed.articles, Field = "CR_AU", type = "matrix", sep = ";") %>% 
  as_tibble()
# build a citing author by cited author matrix and transform to data frame

citing.authors <- strsplit(parsed.articles$AU, ';')

# make a list with all citing authors
authors <- map_chr(citing.authors, function(paper.authors){return(paper.authors[1])})

# get a list of all citing first authors
# add first author of paper as variable in citation matrix and move it as first column

citing.matrix <- citing.matrix %>% 
  mutate(first.author = authors) %>% 
  select(first.author, everything())

# alphabetically order both rows and columns

citing.matrix <- citing.matrix %>% 
  arrange(first.author) %>% 
  filter(first.author != "NA", first.author != "ANONYMOUS") %>% 
  select(-ANONYMOUS, -`NA`)

citing.matrix <- citing.matrix[, !(str_detect(colnames(citing.matrix), "[[:digit:]]"))] %>% 
  select(-`A CORRECTION`, -`A LUDW U FREIB HA`, -`AA VV`, -`AA XV`)


#- BELOW: before adding more data, I cleaned up the citing authors (1500) manually.
# Now it's too much work and not really worth it (original pass consolidated ~10 authors).
# Just applying the SURNAME F method (see shorten.name function).

# 
# # consolidate repeat citing authors
# citing.matrix <- citing.matrix %>% group_by(first.author) %>% summarise_all(sum)
# 
# #remove cited authors with numbers in them
# citing.matrix <- citing.matrix[,!(str_detect(colnames(citing.matrix), "[[:digit:]]"))]
# 
# # manually consolidate repeat authors with more difficult names
# 
# # y %>% write_csv('citing_cited_raw.csv')
# citing.matrix.clean <- read_csv('citing_clean_cited_raw.csv')
# 
# # change author in original matrix with clean version
# citing.matrix$first.author <- citing.matrix.clean$first.author
# 
# # consolidate again
# citing.matrix <- citing.matrix %>% group_by(first.author) %>% summarise_all(sum)

# make a function to shorten the names

shorten.name <- function(x){
    if(str_detect(x, "^ARISTOT[A-Z]+") & !(str_detect(x, "[[:space:]]"))){
      short.name <- "ARISTOTLE"
    }else if(str_detect(x, "^HUSSER[A-Z]*[[:space:]]E[A-Z]*")){
      short.name <- "HUSSERL E"
    }
    if(str_detect(x, "^VAN DER|^VAN DEN|^VON DER")){
      short.name <- str_extract(x, "^[A-Z]+[[:space:]][A-Z]+[[:space:]][A-Z]+[[:space:]]*[[A-Z]]{1}")
    }else if(str_detect(x, "^VAN |^VON ")){
      short.name <- str_extract(x, "^[A-Z]+[[:space:]][A-Z]+[[:space:]]*[[A-Z]]{1}")
    } else{
      short.name <- str_extract(x, "^[A-Z]+[[:space:]]*[[A-Z]]{1}")
    }
    return(short.name)
}

# get all cited author names in SURNAME FIRST INITIAL form (marion j). rougher, but otherwise it's extremely too much work.

short.names <- colnames(citing.matrix)[-1] %>% 
  map_chr(shorten.name)

# to make all data line up, get first authors to SURNAME L form too
short.cited <- map_chr(citing.matrix$first.author, shorten.name)
citing.matrix <- citing.matrix %>% 
  mutate(first.author = short.cited)

# consolidate
citing.matrix <- citing.matrix %>% group_by(first.author) %>% summarise_all(sum)

# allocate new matrix which contains all new data

new.citing.matrix <- matrix(0, nrow =  nrow(citing.matrix), ncol = length(unique(short.names)),dimnames = list(citing.matrix$first.author, unique(short.names)))
colnames(new.citing.matrix) <- unique(short.names)

#transform df into matrix
old.citing.matrix <- citing.matrix[,-1] %>% 
  as.matrix()
row.names(old.citing.matrix) <- citing.matrix$first.author

for(name in sort(colnames(citing.matrix)[-1])){
  # get short version of name
  short.name <- shorten.name(name)
  # sum old name in y with short name in new matrix. this ends up summing multiple versions of the same author in the same column on the new matrix (v.gr. "ADORNO", "ADORNO T" "ADORNO TW" "ADORNO T W" etc)
  new.vector <- new.citing.matrix[, short.name] + old.citing.matrix[, name]
  new.citing.matrix[, short.name] <- new.vector
}

# manually fix some authors (doing regex makes this extremely slow)

aristotle.columns <- colnames(new.citing.matrix) %>% .[str_detect(., "ARISTOT") & !(str_detect(., " "))] 
new.aristotle.column <- apply(X = new.citing.matrix[,aristotle.columns], MARGIN = 1, FUN = sum)
new.citing.matrix <- new.citing.matrix[, !(colnames(new.citing.matrix) %in% aristotle.columns)]
new.citing.matrix <- cbind(new.citing.matrix, new.aristotle.column)
colnames(new.citing.matrix)[ncol(new.citing.matrix)] <- "ARISTOTLE"

husserl.columns <- colnames(new.citing.matrix) %>% .[str_detect(., "HUSSER[A-Z]* E")] 
new.husserl.column <- apply(X = new.citing.matrix[,husserl.columns], MARGIN = 1, FUN = sum)
new.citing.matrix <- new.citing.matrix[, !(colnames(new.citing.matrix) %in% husserl.columns)]
new.citing.matrix <- cbind(new.citing.matrix, new.husserl.column)
colnames(new.citing.matrix)[ncol(new.citing.matrix)] <- "HUSSERL E"

# for the normal one, any author with less than 1 citation (that is, only 1 author cited them only 1 time) is probably noise

new.citing.matrix <- new.citing.matrix[, colSums(new.citing.matrix) > 1]

# remove authors with less than 5 citations

small.citing.matrix <- new.citing.matrix[, colSums(new.citing.matrix) >= 5]

# remove authors who cited only authors with less than 5 citations

small.citing.matrix <- small.citing.matrix[rowSums(small.citing.matrix) >= 5,]

# transform into sparse matrix for saving

new.citing.matrix <- new.citing.matrix %>% 
  Matrix::Matrix(sparse = TRUE)
write(colnames(new.citing.matrix), "complete_matrix_colnames.txt")
write(row.names(new.citing.matrix), "complete_matrix_rownames.txt")
Matrix::writeMM(new.citing.matrix, "complete_matrix.txt")

small.citing.matrix <- small.citing.matrix %>% 
  Matrix::Matrix(sparse = TRUE)
write(colnames(small.citing.matrix), "small_matrix_colnames.txt")
write(row.names(small.citing.matrix), "small_matrix_rownames.txt")
Matrix::writeMM(small.citing.matrix, "small_matrix.txt")

complete.citing.authors <- read.delim("complete_matrix_rownames.txt", header = FALSE)

new.citing.matrix <- new.citing.matrix %>% 
  as.matrix %>% 
  as_tibble() %>% 
  add_column(first.author = complete.citing.authors$V1)

small.citing.authors <- read.delim("small_matrix_rownames.txt", header = FALSE)

small.citing.matrix <- small.citing.matrix %>% 
  as.matrix %>% 
  as_tibble() %>% 
  mutate(first.author = small.citing.authors$V1)

complete.edge.list <- gather(new.citing.matrix, "Target", "Weight", -first.author) %>% 
  rename(Source = first.author) %>% 
  filter(Weight > 0)
write_csv(complete.edge.list, "complete_edge_list.csv")
small.edge.list <- gather(small.citing.matrix, "Target", "Weight", -first.author) %>% 
  rename(Source = first.author) %>% 
  filter(Weight > 0)
write_csv(small.edge.list, "small_edge_list.csv")
