library('bibliometrix')
library('stringr')
library('tidyverse')

files <- readFiles('data/WoS/citations-1-500.txt','data/WoS/citations-501-1000.txt', 
                   'data/WoS/citations-1001-1500.txt', 'data/WoS/citations-1501-2000.txt', 
                   'data/WoS/citations-2001-2500.txt', 'data/WoS/citations-2501-3000.txt',
                   'data/WoS/citations-3001-3500.txt', 'data/WoS/citations-3501-4000.txt',
                   'data/WoS/citations-4001-4500.txt','data/WoS/citations-4501-4543.txt')

# Load raw.articles. View it to see main data.  
# AU is main authors of articles
# CR is citations in the article
# CR_AU is first author listed in each citation
citations_file = "raw_articles.rds"
if (file.exists(citations_file)) {
  raw.articles <- read_rds(citations_file)
} else {
  raw.articles <- convert2df(files,dbsource = "isi", format = "plaintext")
  write_rds(raw.articles, citations_file)
}

# Pulls the cited authors from CR and adds that CR_AU to a new table
parsed.articles <- metaTagExtraction(M = raw.articles, Field = "CR_AU")

# An article-by-cited-author matrix.
# Each cell is a count of how many times an article (row) cited an author (column)
citing.matrix.raw <- cocMatrix(parsed.articles, Field = "CR_AU", type = "matrix", sep = ";") %>% 
  as_tibble()

# For inspection
# write(sort(unique(colnames(citing.matrix.raw))), file = "~/Desktop/citedAuthors.txt")

# Parse out the original authors (AU) and save only the first author in multi-authored pieces
first.author <- map_chr(strsplit(parsed.articles$AU, ';'), function(x){return(x[1])})

# Add first author as first column
citing.matrix.raw <- citing.matrix.raw %>% 
  mutate(first.author) %>%    
  select(first.author, everything())    # Re-order so first.author is first column

# Data cleanup
citing.matrix.clean <- citing.matrix.raw %>% 
  # Alphabetize by first.author
  arrange(first.author) %>%   
  # Filter out rows
  filter(first.author != "NA", !str_detect(first.author,"ANONYMOUS")) %>%
  # Remove NA and Anonymous columns
  select(-ANONYMOUS, -`NA`) %>%                           
  # Remove specific columns
  select(-`A CORRECTION`, -`A LUDW U FREIB HA`, -`AA VV`, -`AA XV`) %>% 
  # Remove Hua columns.  TODO. May be too aggressive and remove Huang, etc.
  select(-matches("Hua*")) %>% 
  # Removing columns with numbers
  select(-matches("[[:digit:]]"))

# For inspection
# write(unlist(unique(citing.matrix.clean[,"first.author"]),use.names = FALSE), 
#       file = "~/Desktop/mainAuthors.txt")
# write(sort(unique(colnames(citing.matrix.clean))), file = "~/Desktop/citedAuthors.txt")

# Create shortened names: e.g. Marion j,
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

# Shorten main author names
short.names <- colnames(citing.matrix.clean)[-1] %>% 
  map_chr(shorten.name)

# To make all data line up, shorted cited author names
short.cited <- map_chr(citing.matrix.clean$first.author, shorten.name)
citing.matrix.clean <- citing.matrix.clean %>% 
  mutate(first.author = short.cited)

# For inspection
# write(sort(unique(short.names)), file = "~/Desktop/shortnames.txt")

# Consolidate
citing.matrix.clean <- citing.matrix.clean %>% 
  group_by(first.author) %>% 
  summarise_all(sum)

# Allocate new matrix which contains all new data
new.citing.matrix <- matrix(0, nrow =  nrow(citing.matrix.clean), ncol = length(unique(short.names)),
                            dimnames = list(citing.matrix.clean$first.author, unique(short.names)))
colnames(new.citing.matrix) <- unique(short.names)

# Transform df into matrix
old.citing.matrix <- citing.matrix.clean[,-1] %>% 
  as.matrix()
row.names(old.citing.matrix) <- citing.matrix.clean$first.author

for(name in sort(colnames(citing.matrix.clean)[-1])){
  # get short version of name
  short.name <- shorten.name(name)
  # sum old name in y with short name in new matrix. this ends up summing multiple versions of the same author in the same column on the new matrix (v.gr. "ADORNO", "ADORNO T" "ADORNO TW" "ADORNO T W" etc)
  new.vector <- new.citing.matrix[, short.name] + old.citing.matrix[, name]
  new.citing.matrix[, short.name] <- new.vector
}

# For inspection (Is ths right thing to look at?)
# write(sort(unique(rownames(new.citing.matrix))), file = "~/Desktop/new_mainAuthors.txt")
# write(sort(unique(colnames(new.citing.matrix))), file = "~/Desktop/new_citedAuthors.txt")

# Manually fix some authors (doing regex makes this extremely slow)
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


# For the normal one, any author with less than 1 citation (that is, only 1 author cited them only 1 time) is probably noise
new.citing.matrix <- new.citing.matrix[, colSums(new.citing.matrix) > 1]

# Remove authors with less than 5 citations
small.citing.matrix <- new.citing.matrix[, colSums(new.citing.matrix) >= 5]

# Remove authors who cited only authors with less than 5 citations
small.citing.matrix <- small.citing.matrix[rowSums(small.citing.matrix) >= 5,]

# Transform into sparse matrix for saving
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

