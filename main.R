# - Main script -----

# Main functions we're using
source('network.R')

# Load data ----

# Citation network. Directed. 
# Format: <Citing, Cited, Number of citations>
citation.edges <- read_csv('data/processed/complete_edge_list.csv') %>%
  rename(weight = Weight) %>% 
  filter(Source != Target) # To remove self-citations

# Co-citation network. Undirected. How often two authors are cited together in the same document
# Format: <Cited author 1, Cited author 2, number of co-occurrences of these authors)
co.citation.edges <- read_csv('data/processed/cocitation_edge_list.csv') %>%
  rename(weight = Weight) %>% 
  filter(Source != Target) # To remove self-citations

# Directed citation network ------

# Create and analyze the citation network
cit.net <- analyze.network(object = citation.edges, directed = TRUE, n.top = 10,
                           layout.function = layout_with_drl, options = drl_defaults$default)
cit.net$visualization
cit.net$stats
cit.net$net.top
# Inspect the igraph nodes in a data frame
View(cit.net$network.df)

# Creates the stats, gephi objects, top members, etc.
write.results(network = cit.net, network.name = "citation_complete")

# Create and analyze sub-networks

husserl.cit.net <- analyze.network(object = cit.net$network.object, target.author = "Husserl E", directed = TRUE)
husserl.cit.net$visualization
write.results(network = husserl.cit.net, network.name = "citation_husserl")

merleauponty.cit.net <- analyze.network(object = cit.net$network.object, target.author = "Merleauponty M", directed = TRUE)
merleauponty.cit.net$visualization
write.results(network = merleauponty.cit.net, network.name = "citation_mp")

heidegger.cit.net <- analyze.network(object = cit.net$network.object, target.author = "Heidegger M", directed = TRUE)
heidegger.cit.net$visualization
write.results(network = heidegger.cit.net, network.name = "citation_heidegger")

# Cocitation network ---------

# Build network

coc.net <- analyze.network(object = co.citation.edges,layout.function = layout_with_drl, options = drl_defaults$default)
coc.net$visualization
coc.net$stats
coc.net$net.top
write.results(network = coc.net, network.name = "cocitation_complete")


# Sub-networks

merleauponty.coc.net <- analyze.network(object = coc.net$network.object, target.author = "Merleauponty M")
merleauponty.coc.net$visualization
write.results(network = merleauponty.coc.net, network.name = "cocitation_mp")


zahavi.coc.net <- analyze.network(object = coc.net$network.object, target.author = "Zahavi D")
zahavi.coc.net$visualization
write.results(network = zahavi.coc.net, network.name = "cocitation_zahavi")

solomon.coc.net <- analyze.network(object = coc.net$network.object, target.author = "Solomon R",
                                   layout.function = layout_with_drl, options = drl_defaults$default)
solomon.coc.net$visualization
write.results(network = solomon.coc.net, network.name = "cocitation_solomon")

wittgenstein.coc.net <- analyze.network(object = coc.net$network.object, target.author = "Wittgenstein L")
wittgenstein.coc.net$visualization
write.results(network = wittgenstein.coc.net, network.name = "cocitation_wittgenstein")

james.coc.net <- analyze.network(object = coc.net$network.object, target.author = "James W")
james.coc.net$visualization
write.results(network = zahavi.coc.net, network.name = "cocitation_james")

husserl.coc.net <- analyze.network(object = coc.net$network.object, target.author = "Husserl E")
husserl.coc.net$visualization
write.results(network = husserl.coc.net, network.name = "cocitation_husserl")