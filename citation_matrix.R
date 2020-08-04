library('bibliometrix')
# For R 4.01 on Mac, earlier versions of dplyr
# Appears to be related to this: https://github.com/tidyverse/dplyr/issues/5017
# For now can be fixed using this:
#     devtools::install_version("dplyr", version = "0.8.5", repos = "http://cran.us.r-project.org")
library('tidyverse') 

# Functions ----

# Load the manual name change document, used by clean.name function
modified.authors <- read_csv(file = "data/processed/name_changes.csv", col_names = c("old.name", "new.name", "comment")) %>% 
  select(old.name, new.name)

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
    # print(x)
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
# CR_AU is first author listed in each citation.  I.e cited author
citations_file = "data/processed/raw_articles.rds"
if (file.exists(citations_file)) {
  raw.articles <- read_rds(citations_file)
} else {
  files <- c('data/WoS/citations1.txt', 'data/WoS/citations2.txt', 
             'data/WoS/citations3.txt', 'data/WoS/citations4.txt', 
             'data/WoS/citations5.txt', 'data/WoS/citations6.txt', 
             'data/WoS/citations7.txt', 'data/WoS/citations8.txt', 
             'data/WoS/citations9.txt', 'data/WoS/citations10.txt', 
             'data/WoS/citations11.txt', 'data/WoS/citations12.txt')
  raw.articles <- convert2df(files,dbsource = "isi", format = "plaintext")
  write_rds(raw.articles, citations_file)
}

# Pulls the cited authors from CR and adds that CR_AU to a new table
parsed.articles <- metaTagExtraction(M = raw.articles, Field = "CR_AU")

# Create and consolidate citation matrix ------

# An article-by-cited-author matrix.
# Each cell is a count of how many times an article (row) cited an author (column)
citing.matrix <- cocMatrix(parsed.articles, Field = "CR_AU", type = "matrix", sep = ";") %>% 
  as_tibble()

# Wrangle the data -----

# Parse out the original authors (AU) and save only the first author in multi-authored pieces
first.author <- map_chr(strsplit(parsed.articles$AU, ';'), function(x){return(x[1])})

# Add first author as first column
citing.matrix <- citing.matrix %>% 
  mutate(first.author = first.author) %>%    
  select(first.author, everything())    # Re-order so first.author is first column

# Initial data cleanup
citing.matrix <- citing.matrix %>% 
  # Alphabetize by first.author
  arrange(first.author) %>%   
  # Filter out rows
  filter(first.author != "NA", !str_detect(first.author,"ANONYMOUS"), !(is.na(first.author))) %>% 
  # Removing columns with numbers
  select(-matches("[[:digit:]]"))

# Shorten main author names
short.cited <- colnames(citing.matrix)[-1] %>%  # Don't use the first column name, which is "first.author"
  map_chr(clean.name) # Returns a vector of strings with the shortened names in the columns
short.cited <- short.cited[!(is.na(short.cited))]

# Shorten citing authors too so we can align authors and citing authors

old.citing <- citing.matrix$first.author
short.citing <- map_chr(citing.matrix$first.author, clean.name)
short.citing <- short.citing[!is.na(short.citing)]

citing.matrix <- citing.matrix %>% 
  mutate(first.author = short.citing)

# Consolidate the columns
# Number of rows is same as citing matrix. 
# Number of columns is number of unique cited authors
citing.matrix.clean <- matrix(
  0,
  nrow =  nrow(citing.matrix),
  ncol = length(unique(short.cited)),
  dimnames = list(citing.matrix$first.author, unique(short.cited))
)

# Transform df into matrix. Temporary table for column consolidation
temp.citing.matrix <- citing.matrix[,-1] %>% 
  as.matrix()
row.names(temp.citing.matrix) <- citing.matrix$first.author

# Now consolidate columns of the citing matrix
# Manually does for columns what the row consolidation does below for row
for(name in sort(colnames(citing.matrix)[-1])){
  # Loop through the names of the cited authors sorted alphabetically
  # Get short version of name
  short.name <- clean.name(name)
  # print(name)
  # print(short.name)
  # Sum old column with short name in new matrix. 
  # this ends up summing multiple versions of the same 
  # author in the same column on the new matrix 
  # (v.gr. "ADORNO", "ADORNO T" "ADORNO TW" "ADORNO T W" etc)
  if(!(is.na(short.name))){
    new.vector <- citing.matrix.clean[, short.name] + temp.citing.matrix[, name]
    citing.matrix.clean[, short.name] <- new.vector
  }
}

# Cleanup: Remove any cited author with less than 1 citation
# (that is, only 1 citing author cited them only 1 time)
# and articles that have only 1 citation (probably noise)
citing.matrix.clean <- citing.matrix.clean[,colSums(citing.matrix.clean) > 1]
citing.matrix.clean <- citing.matrix.clean[rowSums(citing.matrix.clean) > 1,]
# Ignore NA
citing.matrix.clean <- citing.matrix.clean[!(is.na(row.names(citing.matrix.clean))),]
citing.matrix.clean <- citing.matrix.clean[row.names(citing.matrix.clean) != "NA",]
# Sort cited authors
citing.matrix.clean <- citing.matrix.clean[,sort(colnames(citing.matrix.clean))]

# For use in snippets.R
# write_rds(citing.matrix.clean, "citing_matrix.rds")

# Save for co-citation before consolidating citing authors
co.citation.matrix <- citing.matrix.clean
# Remove raw citing for memory
rm(citing.matrix)

citing.matrix.clean <- as_tibble(citing.matrix.clean) %>% 
  add_column(first.author = rownames(citing.matrix.clean))

# Consolidate redundant rows introduced by shortening names
# E.g. sum all "ALEXANDER R" rows into one row
citing.matrix.clean <- citing.matrix.clean %>%
  group_by(first.author) %>%
  summarise_all(sum)

# Make edge lists ------

# Now make the edge list for the network.
# This is a directed edge list, so it's a "long" (vs "wide") version of the matrix.
# See https://data.ca.gov/uploads/page_images/2019-08-28-194741.855848DataPreplong-wide.jpg
# Column 1 is CITING author, column 2 is CITED author, column 3 is number of time CITING author cited CITED author.
# complete.citing.authors <- read.delim("complete_matrix_rownames.txt", header = FALSE)
# Gather turns a wide data frame into a long data frame
complete.edge.list <- gather(citing.matrix.clean, "Target", "Weight", -first.author) %>% 
  rename(Source = first.author) %>% 
  filter(Weight > 0)

# Write the data ----

write_csv(complete.edge.list, "data/processed/complete_edge_list.csv")

# Make co-citation matrix -----

# change to binary to count occurrences
co.citation.matrix[co.citation.matrix > 0] <- 1
# Transpose
co.citation.matrix <- t(co.citation.matrix)
# Next, multiply by its transposed.
# tcrossprod(x) = x %*% t(x)
co.citation.matrix <- tcrossprod(co.citation.matrix)
# This is a citing author x citing author matrix.
# Single co-citations (cell = 1) is probably noise, so get rid of those (replace with 0).
co.citation.matrix[co.citation.matrix == 1] <- 0

# Set the diagonal to 0, it's meaningless.
diag(co.citation.matrix) <- 0

# Clean rows and columns with only 0s
co.citation.matrix <- co.citation.matrix[rowSums(co.citation.matrix) > 0,]
co.citation.matrix <- co.citation.matrix[,colSums(co.citation.matrix) > 0]

# Count the number of non-zero elements per row and column.
# If an author only co-appears with one other author, probably noise.

cutoff <- 1
non.zero <- apply(co.citation.matrix, 1, function(row){
  number.non.zero <- sum(row > 0)
  return(ifelse(number.non.zero <= cutoff, FALSE, TRUE))
  return(sum(row > 0))
})
co.citation.matrix <- co.citation.matrix[non.zero, non.zero]

# Make and save edge list
co.citation.edge.list <- co.citation.matrix %>% 
  as_tibble(rownames = "Source") %>% 
  gather("Target", "Weight", -Source) %>% 
  filter(Weight > 0)
write_csv(co.citation.edge.list, "data/processed/cocitation_edge_list.csv")
