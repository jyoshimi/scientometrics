library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(RColorBrewer)

# Things to fiddle with marked with *

# Preliminary ------

# change this to try other community algorithms. other options: 
# cluster_fast_greedy, cluster_optimal, cluster_infomap.
# To see all the options, type igraph::cluster_ and press `TAB`
detect.community <- igraph::cluster_louvain 

# Load a cooler palette.
getPalette = colorRampPalette(brewer.pal(15, "Set1"))

# Change this to where the files are stored.
# Choices here are small_edge_list.csv and complete_edge_list
edge.list <- read_csv('small_edge_list.csv') %>% 
  rename(weight = Weight)


# Create network ------

# Make an undirected graph first and removing resulting redundant edges with simplify
# This removes the distinction between citing and cited authors
net.undirected <- igraph::graph_from_data_frame(edge.list, directed = FALSE) %>% 
  simplify()


# Run chosen community detection algorithm

net.communities <- detect.community(net.undirected)

# Build a igraph network, which is a directed network built from the edge list
net.directed <- igraph::graph_from_data_frame(edge.list, directed = TRUE)

# Add eigenvector centrality, indegree and community as vertex attributes to the igraph network.
# Used  for plotting.
# Added as attributes to the directed igraph
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "degree", 
                                             value = degree(net.directed, mode = "in"))
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "community", 
                                             value = igraph::membership(net.communities))


# Explore membership ------

# Import as tidynet, which represents graph as a dataframe and it's simpler to visualize it
# This is used for all graphing
tidynet <- tidygraph::as_tbl_graph(net.directed)

# Get nodes as a dataframe. 
# Useful to view
nodes <- tidynet %>% 
  tidygraph::activate(nodes) %>% 
  as_tibble()

# Uncomment to save list of 5 top in-degree authors in each cluster
# use community or community_fg
top.members <- nodes %>%
  group_by(community) %>%
  top_n(5, degree) %>%
  arrange(community, desc(degree)) %>%
  select(name, degree, community) 

# write_csv(top.members, "top_authors_by_community.csv") # uncomment this line to save result to csv


# Visualizing the network ------         

# Add a display.name column which is used to when graphing the network
# Displays a name only if in-degree > 50 (NA means display nothing)
# Note that the > 50 cutoff seems to be a little too low for the big network. *
#
# Also recode community as a categorical variable
tidynet <- tidynet %>%
  tidygraph::activate(nodes) %>% 
  mutate(display.name = ifelse(degree > 50, name, NA),
         community = factor(community))

# Locations of nodes for plotted network. It's saved as a dataframe of x and y
# locations for each node. This is faster than doing it at the moment of
# plotting. layout.drl is the algorithm OpenOrd was based on.
# dlr_defaults$default loads the default configuration for the dlr algorithm.
# Other options change the look drastically! *
net.layout <- igraph::layout.drl(net.directed, options = drl_defaults$coarsen) 

# Plot network using colors for communities, repel the labels so they don't overlap, and don't plot the links. **
net.viz <- ggraph(tidynet, layout = net.layout) +
  geom_node_point(aes(size = degree, color = community)) +
  geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
  scale_colour_manual(values = getPalette(length(unique(nodes$community))))

# Quick view
net.viz

# Uncomment line to save network viz as a pdf.  Can play with resolution and size. *
# ggsave(plot = net.viz, "network_small_lv_degree.pdf", height = 14, width = 14, units = "in", dpi = 500)


# Visualizing sub networks ------

# Function to extract sub network info
get.sub.net <- function(net, author, threshold = 10){
  # Function for recursively analyzing a subnet of a network. The procedure is
  # based on the name of an author. It uses the community structure of the input
  # net to detect a community structure within that sub network using the same
  # algorithm set at the beginning of the code.
  
  # Input:    `net` is a directed igraph object with added vertex attributes for community.
  #           `author` is a string with the name of the target author in 
  #             the form LASTNAME INITIAL e.g. `HUSSERL E`
  #           `threshold` is the threshold of in-degree value at which labels are 
  #           plotted for the authors. default is 10.
  # Returns:  A list.
  #           $igraph.network is a the directed igraph representation of the sub-network,
  #           $nodes.attributtes is a dataframe where each row is an author and 
  #             each column presents in-degree and community,
  #           $visualization is a ggraph plot of the network.
  
  # TODO: Make it less hacky.
  
  author.community <- tidynet %>% 
    tidygraph::activate(nodes) %>% 
    as_tibble() %>% 
    filter(name == author) %>% 
    .$community
  
  author.net.undirected <- induced.subgraph(net, which(V(net)$community == author.community)) %>% 
    as.undirected()
  author.clusters <- detect.community(author.net.undirected)
  author.net.directed <- induced.subgraph(net, which(V(net)$community == author.community)) 
  author.net.directed <- igraph::set.vertex.attribute(graph = author.net.directed, name = "community", 
                                                       value = igraph::membership(author.clusters))
  author.net.directed <- igraph::set.vertex.attribute(graph = author.net.directed, name = "degree", value = degree(author.net.directed, mode = "in"))
  
  tidy.author.net <- tidygraph::as_tbl_graph(author.net.directed)
  
  author.nodes <- tidy.author.net %>% 
    tidygraph::activate(nodes) %>% 
    as_tibble()
  
  tidy.author.net <- tidy.author.net %>%
    tidygraph::activate(nodes) %>% 
    mutate(community = factor(community),
           display.name = ifelse(degree > threshold, name, NA))
  
  author.net.layout <- igraph::layout.drl(author.net.directed, 
                                       options = drl_defaults$default)
  
  author.viz <- ggraph(tidy.author.net, layout = author.net.layout) +
    geom_node_point(aes(size = degree, color = community)) +
    geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
    scale_colour_manual(values = getPalette(length(unique(author.nodes$community))))
  return(list(igraph.network = author.net.directed, 
              nodes.attributes = author.nodes, 
              visualization = author.viz))
}

husserl.net <- get.sub.net(net.directed, "HUSSERL E")
husserl.net$visualization  

marx.net <- get.sub.net(net.directed, "MARX K")
marx.net$visualization  

heidegger.net <- get.sub.net(net.directed, "HEIDEGGER M")
heidegger.net$visualization

sartre.net <- get.sub.net(net.directed, "SARTRE J")
sartre.net$visualization

dreyfus.net <- get.sub.net(net.directed, "DREYFUS H")
dreyfus.net$visualization

searle.net <- get.sub.net(net.directed, "SEARLE J", 5) # Don't understand why this one doesn't show labels.
searle.net$visualization
