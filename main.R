# - Main analysis script -----

# Functions we're using for network analysis
source('functions.R')

# Load data ----

# Citation network. Directed. 
# Format: <Citing, Cited, Number of citations>
citation.edges <- read_csv('data/processed/complete_edge_list.csv') %>%
  rename(weight = Weight) %>% 
  filter(Source != Target) # To remove self-citations

# Directed citation network ------

# Main citation network -----
cit.net <- build.network(object = citation.edges, directed = TRUE)

write.gephi(network = cit.net, network.name = "citation_complete")
write.tops(network = cit.net, network.name = "citation_complete", n.top = 10,
           n.filter = 20)
write.stats(network = cit.net, network.name = "citation_complete")

write.tops(network = cit.net, network.name = "everyone", n.top = 100,
           n.filter = 5)

# Snippets -----

# Get top-10 authors and sort by degree
topAuthors <- cit.net$df %>% top_n(1000,strength) %>% arrange(desc(strength))  
topAuthors
write_csv(x = topAuthors, path = paste0("results/","topAuthors", ".csv"))

# Look at distributios of authors
cit.net$df %>% 
  filter(community %in% c("Husserl", "James")) %>%  
  ggplot(aes(x = strength, y = ..density.., fill = community)) + geom_density(alpha = 0.6) + facet_grid(vars(community))

# Sub-networks (assume cit.net exists) -----

# Husserl
husserl.net <- build.network(object = cit.net$network.object, 
                               target.author = "Husserl E", 
                             directed = TRUE)
write.gephi(network = husserl.net, network.name = "husserl")
write.tops(network = husserl.net, network.name = "husserl")
write.stats(network = husserl.net, network.name = "husserl")
write.tops(network = husserl.net, network.name = "husserl_all", n.top = 100,
           n.filter = 5)


# Heidegger
heidegger.net <- build.network(object = cit.net$network.object, 
                               target.author = "Heidegger M", 
                               directed = TRUE)
write.gephi(network = heidegger.net, network.name = "heidegger")
write.tops(network = heidegger.net, network.name = "heidegger")
write.stats(network = heidegger.net, network.name = "heidegger")
write.tops(network = heidegger.net, network.name = "heidegger_all", n.top = 100,
           n.filter = 5)

# Merleau-Ponty
merleauponty.net <- build.network(object = cit.net$network.object, 
                                  target.author = "Merleauponty M", 
                                  directed = TRUE)
write.gephi(network = merleauponty.net, network.name = "merleauponty")
write.tops(network = merleauponty.net, network.name = "merleauponty")
write.stats(network = merleauponty.net, network.name = "merleauponty")
write.tops(network = merleauponty.net, network.name = "mp_all", n.top = 100,
           n.filter = 5)

# Cocitation networks ---------

# Co-citation networks. Undirected. How often two authors are cited together in the same document
# Format: <Cited author 1, Cited author 2, number of co-occurrences of these authors)
co.citation.edges <- read_csv('data/processed/cocitation_edge_list.csv') %>%
  rename(weight = Weight) %>% 
  filter(Source != Target) # To remove self-citations

# Build co-citation network

coc.net <- build.network(object = co.citation.edges, directed = FALSE)
coc.net$visualization
coc.net$stats
coc.net$net.top
write.results(network = coc.net, network.name = "cocitation_complete")

# Cocitation sub-networks

merleauponty.coc.net <- analyze.network(object = coc.net$network.object, target.author = "Merleauponty M")
merleauponty.coc.net$visualization
write.results(network = merleauponty.coc.net, network.name = "cocitation_mp")
