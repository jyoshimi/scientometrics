library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

setwd('./ipython/scientometrics/')

# Change this to where the files are stored. Small_edge_list.csv has the limited network (in-degree and out-degree filtering)
db <- read_csv('small_edge_list.csv') %>% 
  rename(weight = Weight)

# Make an undirected graph first and simplify it to remove redundant links
net <- igraph::graph_from_data_frame(db, directed = FALSE) %>% 
  simplify()

# Get eigenvector centrality
eigenvectors <- igraph::eigen_centrality(net) %>% .$vector

# Try both louvain and F&G Clustering
lv.cluster <- igraph::cluster_louvain(net)
fg.cluster <- igraph::cluster_fast_greedy(net)

# Build an igraph
net <- igraph::graph_from_data_frame(db, directed = TRUE)

# Add eigenvector centrality, indegree and community to the graph for plotting.
net <- igraph::set.vertex.attribute(graph = net, name = "eigen", value =  eigenvectors)
net <- igraph::set.vertex.attribute(graph = net, name = "degree", value = degree(net, mode = "in"))
# Change this line to use either fg.cluster or lv.cluster
net <- igraph::set.vertex.attribute(graph = net, name = "community", 
                                    value = igraph::membership(lv.cluster))

# Import as tidynet, which represents graph as a dataframe and it's simpler to visualize it
tidynet <- tidygraph::as_tbl_graph(net)

# Get nodes as a dataframe
nodes <- tidynet %>% 
  tidygraph::activate(nodes) %>% 
  as_tibble()

# Uncomment next section to save list of 5 top in-degree authors in each cluster to a filename (set in line 42)
# nodes %>%
#   group_by(community) %>%
#   top_n(5, degree) %>%
#   arrange(community, desc(degree)) %>%
#   select(-eigen) %>%
#   write_csv("lv_complete_degree_directed.csv")

# Get the tidynet with added clustering
# Set a "display.name" so we can show only the authors with an indegree > 50 (arbitrary).
# The > 50 cutoff seems to be a little too low for the big network.
tidynet <- tidynet %>%
  tidygraph::activate(nodes) %>% 
  mutate(community = factor(community),
         display.name = ifelse(degree > 50, name, NA))

# This determines which layout the network will have when plotted. It's saved as a dataframe of x and y locations for each node. This is faster than doing it at the moment of plotting.
# layout.drl is the algorithm in which OpenOrd was based.
# dlr_defaults$default loads the default configuration for the dlr algorithm. Other options change the look drastically!
net.layout <- igraph::layout.drl(net, options = drl_defaults$coarsen)

# Load a cooler palette
library(RColorBrewer)

# This function interpolates a palette with a higher number of colors than the original SET1
# (Ignore warning here)
getPalette = colorRampPalette(brewer.pal(15, "Set1"))

# Plot network using colors for communities, repel the labels so they don't overlap, and don't plot the links.
net.viz <- ggraph(tidynet, layout = net.layout) +
  geom_node_point(aes(size = degree, color = community)) +
  geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
  scale_colour_manual(values = getPalette(length(unique(nodes$community))))

net.viz

# Save network viz as a pdf.
ggsave(plot = net.viz, "network_small_lv_degree.pdf", scale = 2, dpi = 500)

# Explore smaller husserl network

husserl.cluster <- nodes %>%
  filter(name == "HUSSERL E") %>% 
  .$community

husserl.net.undirected <- induced.subgraph(net, which(V(net)$community == husserl.cluster)) %>% 
  as.undirected()
husserl.clusters <- igraph::cluster_louvain(husserl.net.undirected)
husserl.net.directed <- induced.subgraph(net, which(V(net)$community == husserl.cluster)) 
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
