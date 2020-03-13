library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

# Things to fiddle with marked with *

# Change this to where the files are stored.
# Choices here are small_edge_list.csv and complete_edge_list
db <- read_csv('small_edge_list.csv') %>% 
  rename(weight = Weight)

# Make an undirected graph first and removing resulting redundant edges with simplify
# This removes the distinction between citing and cited authors
net.undirected <- igraph::graph_from_data_frame(db, directed = FALSE) %>% 
  simplify()

# Get eigenvector centrality of authors
eigenvectors <- igraph::eigen_centrality(net.undirected) %>% .$vector

# Try both louvain and F&G Clustering. Returns lists of communities.
lv.cluster <- igraph::cluster_louvain(net.undirected)
fg.cluster <- igraph::cluster_fast_greedy(net.undirected)

# Build a igraph network, which is a directed network built from the edge list
net.directed <- igraph::graph_from_data_frame(db, directed = TRUE)

# Add eigenvector centrality, indegree and community as vertex attributes to the igraph network.
# Used  for plotting.
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "eigen", value =  eigenvectors)
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "degree", value = degree(net.directed, mode = "in"))
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "community_lv", 
                                             value = igraph::membership(lv.cluster))
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "community_fg", 
                                             value = igraph::membership(fg.cluster))

# Import as tidynet, which represents graph as a dataframe and it's simpler to visualize it
# This is used for all graphing
tidynet <- tidygraph::as_tbl_graph(net.directed)

# Get nodes as a dataframe. 
# Useful to view
nodes <- tidynet %>% 
  tidygraph::activate(nodes) %>% 
  as_tibble()

# Save list of 5 top in-degree authors in each cluster
# use community_lv or community_fg
# nodes %>%
#   group_by(community_lv) %>%
#   top_n(5, degree) %>%
#   arrange(community_lv, desc(degree)) %>%
#   select(name, degree, community_lv) %>%
#   write_csv("top_authors_by_community.csv")

         
# Add a display.name column which is used to when graphing the network
# Displays a name only if in-degree > 50 (NA means display nothing)
# Note that the > 50 cutoff seems to be a little too low for the big network. *
#
# Also recode community as a categorical variable
tidynet <- tidynet %>%
  tidygraph::activate(nodes) %>% 
  mutate(display.name = ifelse(degree > 50, name, NA),
         community_lv = factor(community_lv))

# Locations of nodes for plotted network. It's saved as a dataframe of x and y
# locations for each node. This is faster than doing it at the moment of
# plotting. layout.drl is the algorithm OpenOrd was based on.
# dlr_defaults$default loads the default configuration for the dlr algorithm.
# Other options change the look drastically! *
net.layout <- igraph::layout.drl(net.directed, options = drl_defaults$coarsen) 

# Load a cooler palette.
library(RColorBrewer)

# This function interpolates a palette with a higher number of colors than the original "Set1"
# (Ignore warning here)
getPalette = colorRampPalette(brewer.pal(15, "Set1"))

# Plot network using colors for communities, repel the labels so they don't overlap, and don't plot the links. **
net.viz <- ggraph(tidynet, layout = net.layout) +
  geom_node_point(aes(size = degree, color = community)) +
  geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
  scale_colour_manual(values = getPalette(length(unique(nodes$community))))

# Quick view
# net.viz

# Save network viz as a pdf.  Can play with resolution and size. *
ggsave(plot = net.viz, "network_small_lv_degree.pdf", scale = 2, dpi = 500)

# Explore smaller husserl network.  
# TODO: Pablo will turn this into a function
husserl.cluster <- nodes %>%
  filter(name == "HUSSERL E") %>% 
  .$community

husserl.net.undirected <- induced.subgraph(net.directed, which(V(net.directed)$community == husserl.cluster)) %>% 
  as.undirected()
husserl.clusters <- igraph::cluster_louvain(husserl.net.undirected)
husserl.net.directed <- induced.subgraph(net.directed, which(V(net.directed)$community == husserl.cluster)) 
husserl.net.directed <- igraph::set.vertex.attribute(graph = husserl.net.directed, name = "community", 
                                    value = igraph::membership(husserl.clusters))
husserl.net.directed <- igraph::set.vertex.attribute(graph = husserl.net.directed, name = "degree", value = degree(husserl.net.directed, mode = "in"))

tidy.husserl <- tidygraph::as_tbl_graph(husserl.net.directed)

# Get nodes as a dataframe
husserl.nodes <- tidy.husserl %>% 
  tidygraph::activate(nodes) %>% 
  as_tibble()
husserl.nodes %>%
  group_by(community) %>%
  top_n(5, degree) %>%
  arrange(community, desc(degree)) %>%
  select(-eigen) %>%
  write_csv("husserl_small.csv")


tidy.husserl <- tidy.husserl %>%
  tidygraph::activate(nodes) %>% 
  mutate(community = factor(community),
         display.name = ifelse(degree > 15, name, NA))

husserl.layout <- igraph::layout.drl(husserl.net.directed, 
                                     options = drl_defaults$default)

husserl.viz <- ggraph(tidy.husserl, layout = husserl.layout) +
  geom_node_point(aes(size = degree, color = community)) +
  geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
  scale_colour_manual(values = getPalette(length(unique(husserl.nodes$community))))

husserl.viz
ggsave(plot = husserl.viz, "husserl_small.pdf", scale = 2, dpi = 500)
