library('bibliometrix')
library('tidyverse') 
library(ggraph)

# Load  main objects of interest, all as dataframes ----
# (Snippets that depend on the citation network are in main.R)

# Web of science data
#
# For a list use "colnames(articles)" and see
# https://images.webofknowledge.com/images/help/WOS/hs_wos_fieldtags.html
#
#  We added CR_AU which is a list of all cited authors
#
articles <- metaTagExtraction(M = read_rds("data/processed/raw_articles.rds"), Field = "CR_AU")

# An article-by-cited-author matrix.  (TODO: Is this description correct?)
# Each cell is a count of how many times a citing author (row) cites another author (column)
#  TODO: Can we dispense with this one and use the others?
citation_matrix <- read_rds("citing_matrix.rds")

# Source, Target, Weight(Number of citations)
edges <- as_tibble(read_csv('data/processed/complete_edge_list.csv'))
  
# Snippets ----

# Number of appearances as citing author
sum(row.names(citation_matrix) == "YOSHIMI J")

# Find number of appearances as cited author.  TODO: Broken?
sum(colnames(citation_matrix) == "YOSHIMI J")

# Get article titles with a specific citing author.
articles %>% filter(str_detect(AU,"ZANG")) %>% select(PY, JI, TI) %>%  print()

# Get article titles with a specific cited author. 
articles %>% filter(str_detect(CR, "CASSIRER")) %>%  select(AU,  PY, JI, TI, DE, ID) %>%   View()

# Who are these anonyhmous folks?
articles %>% filter(str_detect(AU, "ANONYMOUS")) %>%  select(AU,  PY, JI, TI, DE, ID) %>%   View()

# Find out-degree of a specific author. 
edges %>% filter(Source == "YOSHIMI J") %>% select(Weight) %>% count()

# Find in-degree of a specific author.
edges %>% filter(Target == "YOSHIMI J") %>% select(Weight) %>% count()

# Create a list of every cited author
sort(unique(colnames(citation_matrix)))

# Get top-10 authors and sort by degree
citation_matrix %>% top_n(1000,strength)


# Number of articles grabbed from each journal
articles %>%  group_by(LA) %>% tally() %>% 
  arrange(desc(n)) %>% 
  mutate(LA = str_to_title(LA))

# Number of articles grabbed from each journal
articles %>%  group_by(SO) %>% tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  filter(proportion > .01) %>% 
  arrange(desc(n)) %>% 
  mutate(SO = str_to_title(SO))

# Histogram of number of articles per year
ggplot(data = articles, aes(x = PY)) + 
  geom_histogram(bins = 20, color = "white") + labs(x="Year", y = "Count")
mean(articles$PY, na.rm = TRUE)
sd(articles$PY, na.rm = TRUE)

# Make sure we aren't getting an author because of JCS. Can plug in Gallagher too.
total_pcs <- articles %>% 
  filter(SO == "PHENOMENOLOGY AND THE COGNITIVE SCIENCES" ) %>%  
  nrow()
total_zahavi <- articles %>% 
  filter(SO == "PHENOMENOLOGY AND THE COGNITIVE SCIENCES" & str_detect(CR_AU, "ZAHAVI")) %>%
  nrow() 
print(c(total_pcs, total_zahavi, total_pcs - total_zahavi))

# See how often "phenomenology of spirit" occurs
# 232 vs. over 1K in the Kant/Hegel cluster.  
# So the title plays a clear role
PS = "PHENOMENOLOGY OF SPIRIT|PHENOMENOLOGY OF MIND|DES GEISTES"
hegel.folks <- articles %>%
  filter(str_detect(AB, PS) | str_detect(TI, PS) | str_detect(DE, PS)) %>% 
  select(AU, TI)
nrow(hegel.folks)
View(hegel.folks) # Peruse titles and references. Suggests this really is a mainly Hegel discussion

