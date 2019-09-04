files <- bibliometrix::readFiles('data/Scopus/phenomenology-1970-2000.bib', 'data/Scopus/phenomenology-2001-2010.bib',
                                 'data/Scopus/phenomenology-2011-2014.bib', 'data/Scopus/phenomenology-2015-2017.bib',
                                 'data/Scopus/phenomenology-2018-2019.bib')



raw.articles <- bibliometrix::convert2df(files, dbsource = "scopus", format = "bibtex")
parsed.articles <- metaTagExtraction(M = raw.articles, Field = "CR_AU")
citing.matrix <- cocMatrix(parsed.articles, Field = "CR_AU", type = "matrix", sep = ";") %>% 
  as_tibble()
citing.authors <- strsplit(parsed.articles$AU, ';')

authors <- map_chr(citing.authors, function(paper.authors){return(paper.authors[1])})
# get a list of all citing first authors

citing.matrix <- citing.matrix %>% 
  mutate(first.author = authors) %>% 
  select(first.author, everything())
# add first author of paper as variable in citation matrix and move it as first column

citing.matrix <- citing.matrix %>% 
  arrange(first.author) %>% 
  filter(first.author != "NA", first.author != "ANONYMOUS")

citing.matrix <- citing.matrix %>% group_by(first.author) %>% summarise_all(sum)
citing.matrix <- citing.matrix[,!(str_detect(colnames(citing.matrix), "[[:digit:]]"))]

# the cleaning loop fails immediately because of how messy the names are, like    
# [2] "A ARTAUD" or
#    [6] "A COMMENT ON THESE TWO TERMSREPRESENTATION AND EXPRESSIONSEEMS CALLED
#    FOR I USE REPRESENTATION IN A GENERAL SENSE TO REFER TO WHAT AN ARTIST
#    ACHIEVES WHEN HE OR SHE GIVES FORM TO EXPERIENCE OR THOUGHT"

short.names <- colnames(citing.matrix)[-1] %>% 
  map_chr(function(x){
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
  })


# to make all data line up, get first authors to SURNAME L form too

citing.matrix <- citing.matrix %>% 
  mutate(first.author = str_extract(first.author, "^[A-Z]+[[:space:]]*[[A-Z]]{1}"))
citing.matrix <- citing.matrix %>% group_by(first.author) %>% summarise_all(sum)

# allocate new matrix which contains all new data

new.citing.matrix <- matrix(0, nrow =  nrow(citing.matrix), ncol = length(unique(short.names)),dimnames = list(citing.matrix$first.author, unique(short.names)))
colnames(new.citing.matrix) <- unique(short.names)

#transform df into matrix
old.citing.matrix <- citing.matrix[,-1] %>% 
  as.matrix()
row.names(old.citing.matrix) <- citing.matrix$first.author
