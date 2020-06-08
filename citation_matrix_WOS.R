library('bibliometrix')
library('stringr')
library('tidyverse')

# Functions ----

# Load the manual name change document, used by clean.name function
modified.authors <- read_csv(file = "name_changes.csv") %>% 
  select(old.name = `OLD NAME`, new.name = `NEW NAME ("NA" to Delete)`)

# Function to clean up names, by shortening them and handling special cases in a manual
# name change document (name_changes.csv). This ensures multiple instances of a name
# are all coded in a common format.  The symbol "NA" is used for names to exclude.
# 
# Input:
#       `x` is a string with a full name of an author, ideally in the form LASTNAME FIRSTNAME
# Returns:
#       A string with a shortened version of that name, for most cases as LASTNAME INITIAL
#
# Examples: 
#       HEIDEGGER MARTIN      -> HEIDEGGER M 
#       GUSSERL               -> HUSSERL E
#       ALCOHOLICS ANONYMOUS  -> NA
#
# Feel free to add comments to the manual name change document 
# https://docs.google.com/spreadsheets/d/14aVjy5vOlNBvnlhcSt5rDbX4conwx4bP6EL1CQkyL2c/edit?usp=sharing
clean.name <- function(x) {
  
  # Ignore NAs
  if (is.na(x)) {
    return(NA)
  }
  if(x == "NA"){return(NA)}
  
  # Manual name changes.  
  if(x %in% modified.authors$old.name){
    name.ind <- which(modified.authors$old.name == x)
    # print(x)s
    return(modified.authors$new.name[name.ind])
  }
  else{
    # The main algorithm applied to all authors Special cases are above.
    # Extracts the longest string followed by a space and then the
    # first letter of the next string
    return(str_extract(x, "^[A-Z]+[[:space:]]*[[A-Z]]{1}"))
  }
}

# Load and parse articles ------

# Load raw.articles. View it to see main data. 
# AU is citing author
# CR is citations in the article
# CR_AU is first author listed in each citation.  I..e cited author
citations_file = "raw_articles.rds"
if (file.exists(citations_file)) {
  raw.articles <- read_rds(citations_file)
} else {
  files <- c('data/WoS/citations-1-500.txt','data/WoS/citations-501-1000.txt', 
                     'data/WoS/citations-1001-1500.txt', 'data/WoS/citations-1501-2000.txt', 
                     'data/WoS/citations-2001-2500.txt', 'data/WoS/citations-2501-3000.txt',
                     'data/WoS/citations-3001-3500.txt', 'data/WoS/citations-3501-4000.txt',
                     'data/WoS/citations-4001-4500.txt','data/WoS/citations-4501-4543.txt')
  raw.articles <- convert2df(files,dbsource = "isi", format = "plaintext")
  write_rds(raw.articles, citations_file)
}

# Pulls the cited authors from CR and adds that CR_AU to a new table
parsed.articles <- metaTagExtraction(M = raw.articles, Field = "CR_AU")

# Create and consolidate citation matrix ------

# An article-by-cited-author matrix.
# Each cell is a count of how many times an article (row) cited an author (column)
citing.matrix.raw <- cocMatrix(parsed.articles, Field = "CR_AU", type = "matrix", sep = ";") %>% 
  as_tibble()

# Useful code snippets ------
# write(sort(unique(colnames(citing.matrix.raw))), file = "~/Desktop/citedAuthors.txt")

# See number of articles grabbed from each journal
# raw.articles %>%  group_by(SO) %>% tally() %>% View()

# Make sure we aren't getting Zahavi because of JCS. Can plug in gallagher too.
# parsed.articles %>% 
#   filter(SO == "PHENOMENOLOGY AND THE COGNITIVE SCIENCES" & str_detect(CR_AU, "ZAHAVI")) %>% 
#   nrow()

# See how often "phenomenology of spirit" is what was found
# Results: Total: 189, Abstracts (AB): 113, Title (TI): 97,  Keywords (DE): 21
# Note that there are 297 citing authors in the "Hegel cluster"
# PS = "PHENOMENOLOGY OF SPIRIT|PHENOMENOLOGY OF MIND|DES GEISTES"
# hegel.folks <- parsed.articles %>%
#   filter(str_detect(AB, PS) | str_detect(TI, PS) | str_detect(DE, PS)) 
#   # .$AU %>%  unique()
# nrow(hegel.folks)
# View(hegel.folks) # Peruse titles and references. Suggests this really is a mainly Hegel discussion

# Wrangle the data -----

# Parse out the original authors (AU) and save only the first author in multi-authored pieces
first.author <- map_chr(strsplit(parsed.articles$AU, ';'), function(x){return(x[1])})

# Add first author as first column
citing.matrix.raw <- citing.matrix.raw %>% 
  mutate(first.author) %>%    
  select(first.author, everything())    # Re-order so first.author is first column

# Initial data cleanup
citing.matrix.clean <- citing.matrix.raw %>% 
  # Alphabetize by first.author
  arrange(first.author) %>%   
  # Filter out rows
  filter(first.author != "NA", !str_detect(first.author,"ANONYMOUS"), !(is.na(first.author))) %>% 
  # Removing columns with numbers
  select(-matches("[[:digit:]]"))
  
# Snippet to create a list of cited authors
# write(unlist(unique(citing.matrix.clean[,"first.author"]),use.names = FALSE), 
#       file = "~/Desktop/mainAuthors.txt")
# write(sort(unique(colnames(citing.matrix.clean))), file = "~/Desktop/citedAuthors.txt")

# Shorten main author names
short.cited <- colnames(citing.matrix.clean)[-1] %>%  # Don't use the first column name, which is "first.author"
  map_chr(clean.name) # Returns a vector of strings with the shortened names in the columns

short.cited <- short.cited[!(is.na(short.cited))]

# Shorten citing authors too so we can align authors and citing authors
short.citing <- map_chr(citing.matrix.clean$first.author, clean.name)
citing.matrix.clean <- citing.matrix.clean %>% 
  mutate(first.author = short.citing)

citing.matrix.clean <- citing.matrix.clean %>% 
  filter(!(is.na(first.author)),
         first.author != "NA") 

# Snippet to inspect shortened names
# write(sort(unique(short.names)), file = "~/Desktop/shortnames.txt")

# Consolidate redundant rows introduced by shortening names
# E.g. sum all "ALEXANDER R" rows into one row
citing.matrix.clean <- citing.matrix.clean %>% 
  group_by(first.author) %>% 
  summarise_all(sum)

# Pre-allocate a new matrix with consolidated columns.
# Number of rows is same as citing matrix. 
# Number of columns is number of unique cited authors
new.citing.matrix <- matrix(
  0,
  nrow =  nrow(citing.matrix.clean),
  ncol = length(unique(short.cited)),
  dimnames = list(citing.matrix.clean$first.author, unique(short.cited))
)

# Transform df into matrix. Temporary table for column consolidation
temp.citing.matrix <- citing.matrix.clean[,-1] %>% 
  as.matrix()
row.names(temp.citing.matrix) <- citing.matrix.clean$first.author

# Now consolidate the COLUMNS of the citing matrix
# Manully does for columns what the row consolidation above does for row
for(name in sort(colnames(citing.matrix.clean)[-1])){
  # Loop through the names of the cited authors sorted alphabetically
  # Get short version of name
  short.name <- clean.name(name)
  print(short.name)
  # Sum old column with short name in new matrix. 
  # this ends up summing multiple versions of the same 
  # author in the same column on the new matrix 
  # (v.gr. "ADORNO", "ADORNO T" "ADORNO TW" "ADORNO T W" etc)
  if(!(is.na(short.name))){
    new.vector <- new.citing.matrix[, short.name] + temp.citing.matrix[, name]
    new.citing.matrix[, short.name] <- new.vector
    }
}

# Snippets for inspection
# write(sort(unique(rownames(new.citing.matrix))), file = "~/Desktop/new_mainAuthors.txt")
# write(sort(unique(colnames(new.citing.matrix))), file = "~/Desktop/new_citedAuthors.txt")

# Cleanup: Remove any cited author with less than 1 citation
# (that is, only 1 citing author cited them only 1 time)
new.citing.matrix <- new.citing.matrix[, colSums(new.citing.matrix) > 1]
new.citing.matrix <- new.citing.matrix[,sort(colnames(new.citing.matrix))]

# Create small matrix ------

small.threshold <- 5

# For the small matrix, remove cited authors with less than small.threshold citations
small.citing.matrix <- new.citing.matrix[, colSums(new.citing.matrix) >= small.threshold]
# Also remove citing authors who cited only authors with less than small.threshold citations
# That is, citing authors with only 0 now in their whole row
small.citing.matrix <- small.citing.matrix[rowSums(small.citing.matrix) >= 0,]

# Save matrices ------

# Transform both matrices into sparse for backup saving 
# This uses the Matrix package
# To preserve column and row names, you have to save them separately
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


# Make edge lists ------

# Now make the edge list for the network.
# This is a directed edge list, so it's a "long" (vs "wide") version of the matrix.
# See https://data.ca.gov/uploads/page_images/2019-08-28-194741.855848DataPreplong-wide.jpg
# Column 1 is CITING author, column 2 is CITED author, column 3 is number of time CITING author cited CITED author.
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
# Gather turns a wide data frame into a long data frame
complete.edge.list <- gather(new.citing.matrix, "Target", "Weight", -first.author) %>% 
  rename(Source = first.author) %>% 
  filter(Weight > 0)
small.edge.list <- gather(small.citing.matrix, "Target", "Weight", -first.author) %>% 
  rename(Source = first.author) %>% 
  filter(Weight > 0)

# These are the things we will use in the network analysis
write_csv(complete.edge.list, "complete_edge_list.csv")
write_csv(small.edge.list, "small_edge_list.csv")

