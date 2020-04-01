library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(RColorBrewer)

# Things to fiddle with marked with *

# Preliminary ------

# Load a cooler palette.
# getPalette = colorRampPalette(brewer.pal(15, "Set1"))  # *
color.palette <- c("#67bf46", "#5150fc", "#415900", "#d300c7", "#433c09",
                   "#6687ff", "#d30057", "#0065a3", "#aa6985",
                    "black", "blue", "yellow", "green", "gray", "pink")

# Meaningful structure from fastgreedy.community, louvain, walktrap, and spinglass
# All others return few commnities or take a long time to run
detect.community <- igraph::cluster_spinglass

# Change this to where the files are stored.
# Choices here are small_edge_list.csv and complete_edge_list.csv
edge.list <- read_csv('complete_edge_list.csv') %>%
  rename(weight = Weight)


# Create network ------

# Make an undirected graph first and removing resulting redundant edges with simplify
# This removes the distinction between citing and cited authors.
# Ignore warning
net.undirected <- igraph::graph_from_data_frame(edge.list, directed = FALSE) %>%
  simplify()

# Build a igraph network, which is a directed network built from the edge list
net.directed <- igraph::graph_from_data_frame(edge.list, directed = TRUE)


# Run chosen community detection algorithm
# * Must change depending on which detection alg is used
net.communities <- detect.community(net.undirected)

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

# Get nodes as a dataframe
# Useful to view
nodes <- tidynet %>%
  tidygraph::activate(nodes) %>%
  as_tibble()

# Get top by indegree
tops <- nodes %>% top_n(50,degree) %>% 
  arrange(desc(degree))
write_csv(tops,"tops.csv")

# Uncomment to save list of 5 top in-degree authors in each cluster
top.members <- nodes %>%
  group_by(community) %>%
  top_n(5, degree) %>%
  arrange(community, desc(degree)) %>%
  select(name, degree, community)
write_csv(top.members, "top_authors_by_community.csv") # uncomment this line to save result to csv


# Visualizing the network ------

# Add a display.name column which is used to when graphing the network
# Displays a name only if in-degree > 50 (NA means display nothing)
# Note that the > 50 cutoff seems to be a little too low for the big network. *
#
# Also recode community as a categorical variable
tidynet <- tidynet %>%
  tidygraph::activate(nodes) %>%
  mutate(display.name = ifelse(degree > 100, name, NA),
         community = factor(community))

# Exaggerate community structure (not used for all)
# Rescale within and between community weights 
# Inspired by https://stackoverflow.com/questions/16390221/how-to-make-grouped-layout-in-igraph
edge.weights <- function(community, network, weight.within = 1.5, weight.between = 1) {
  bridges <- crossing(communities = community, graph = network)
  weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
  return(weights)
}
# change below to directed or undirected *
group.edges <- edge.weights(net.communities, net.directed) 

# Layout sets locations of nodes for plotted network. 
# It's saved as a dataframe of x and y locations for each node. 
# This is faster than doing it at the moment of
# plotting. layout.drl is the algorithm OpenOrd is based on.
# dlr_defaults$default loads the default configuration for the dlr algorithm.
# Other options change the look drastically! **
# Comment/uncomment below
# net.layout <- igraph::layout.fruchterman.reingold(net.directed, weights=E(net.directed)$weight)
# net.layout <- igraph::layout.auto(net.directed, weights=E(net.directed)$weight)
net.layout <- igraph::layout.drl(net.directed, options = drl_defaults$default, weights = (E(net.directed)$weight * group.edges))

# Plot network using colors for communities, repel the labels so they don't overlap, and don't plot the links. **
# This uses the modified weights to exaggerate the structure.
net.viz <- ggraph(tidynet, layout = net.layout) +
  geom_node_point(aes(size = degree, color = community)) +
  geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
  scale_colour_manual(values = getPalette(length(unique(nodes$community))))

# Quick view
net.viz

# Uncomment line to save network viz as a pdf.  Can play with resolution and size. *
ggsave(plot = net.viz, "citation_network.pdf", height = 14, width = 14, units = "in", dpi = 500)

# Uncomment to save for Gephi
# TODO: Pablo have a look?
# gephi = igraph.to.gexf(net.directed, position = net.layout)

#
# Hive stuff------
#

# This hacky function generates a field for each edge that's 0 if it's a within-community edge. 
# Useful (I think??) for the hive plot. Probably a better way of doing this.
# TODO Move this below?
opposite.weights <- function(community, network, weight.within = 0, weight.between = 1) {
  bridges <- crossing(communities = community, graph = network)
  weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
  return(weights)
}

tidynet <- tidynet %>%
  activate(edges) %>%
  mutate(inverse.weight = (E(net.directed)$weight * opposite.weights(net.communities, net.directed)) ^ 2)

# Use this to fine tune the hive display *
small.tidy <- tidynet %>%
  activate(nodes) %>%
  # Labels
  mutate(display.name = ifelse(degree > 75, name, NA)) %>%
  # Delete nodes with low indegree
  filter(degree > 15) %>% 
  # Exclude = TRUE gets rid of unused factor levels
  mutate(community = factor(as.numeric(community), labels = c(1:length(unique(community))), exclude = TRUE)) %>%
  activate(edges) %>%
  # This is where the within-community edges are eliminated
  filter(weight > 2, inverse.weight > 0)

# Get a vector with the community of each node and add it as a field to plot the hive
community.vector <- small.tidy %>%
  activate(nodes) %>%
  as_tibble %>%
  droplevels() %>%
  .$community
small.tidy <- small.tidy %>%
  activate(edges) %>%
  mutate(community = community.vector[from])

# Plot the hive. Each axis is a community. 
# Generate arrows for between community edges larger than 2 to display relationships between communities.
hive.plot <- ggraph(small.tidy, layout = 'hive', axis = community, sort.by = degree) +
  geom_edge_hive(aes(width = log(weight), edge_color = community, alpha = log(weight)), arrow = arrow(length = unit(4, 'mm')),
                 end_cap = circle(1, 'mm')) +
  geom_node_label(aes(label = display.name, size = log(degree), color = community), repel = TRUE) +
  geom_axis_hive() +
  coord_fixed() +
  scale_color_manual(values = c(color.palette, "red", "blue")) +
  scale_edge_color_manual(values = c(color.palette, "red", "blue"), limits = levels(community.vector))

# Quick view
hive.plot

# Uncomment line to save network viz as a pdf.  Can play with resolution and size. *
ggsave(plot = hive.plot, "hive_plot.pdf", height = 14, width = 14, units = "in", dpi = 500)


# Visualizing sub networks ------

# TODO: Fix for undirected case

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
  author.net.directed <- induced.subgraph(net, which(V(net)$community == author.community))
  author.clusters <- detect.community(author.net.directed)

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
  tidy.author.net <- tidy.author.net %>%
    activate(edges) %>%
    mutate(inverse.weight = (E(author.net.directed)$weight * opposite.weights(author.clusters, author.net.directed)) ^ 2)
  # author.net.layout <- igraph::layout.drl(author.net.directed,
  # options = drl_defaults$default)

  author.group.edges <- edge.weights(author.clusters, author.net.directed)

  author.net.layout <- igraph::layout.drl(author.net.directed, options = drl_defaults$default, weights = (E(author.net.directed)$weight * author.group.edges))

  author.viz <- ggraph(tidy.author.net, layout = author.net.layout) +
    geom_node_point(aes(size = degree, color = community)) +
    geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
    scale_colour_manual(values = getPalette(length(unique(author.nodes$community))))

  small.tidy.author <- tidy.author.net %>%
    activate(nodes) %>%
    mutate(display.name = ifelse(degree > 15, name, NA)) %>%
    filter(degree > 5) %>%
    mutate(community = factor(as.numeric(community), labels = c(1:length(unique(community))), exclude = TRUE)) %>%
    activate(edges) %>%
    filter(weight > 2, inverse.weight > 0)
  author.community.vector <- small.tidy.author %>%
    activate(nodes) %>%
    as_tibble %>%
    droplevels() %>%
    .$community
  small.tidy.author <- small.tidy.author %>%
    activate(edges) %>%
    mutate(community = author.community.vector[from])

  author.hive <- ggraph(small.tidy.author, layout = 'hive', axis = community, sort.by = degree) +
    geom_edge_hive(aes(width = log(weight), edge_color = community, alpha = log(weight)), arrow = arrow(length = unit(4, 'mm')),
                   end_cap = circle(1, 'mm')) +
    geom_node_label(aes(label = display.name, size = log(degree), color = community), repel = TRUE) +
    geom_axis_hive() +
    coord_fixed() +
    scale_color_manual(values = color.palette) +
    scale_edge_color_manual(values = color.palette, limits = levels(author.community.vector))

  return(list(igraph.network = author.net.directed,
              nodes.attributes = author.nodes,
              visualization = author.viz,
              hive = author.hive))
}

# Manually use the get.sub.net function for communities of interest

# Husserl sub-community
husserl.net <- get.sub.net(net.directed, "HUSSERL E")
husserl.net$visualization # Network plot object
husserl.net$hive # Hive plot object
husserl.top.members <- husserl.net$nodes.attributes %>%
  group_by(community) %>%
  top_n(5, degree) %>%
  arrange(community, desc(degree)) %>%
  select(name, degree, community)
write_csv(husserl.top.members, "husserl_communities.csv")
ggsave(plot = husserl.net$hive, "husserl_hive.pdf", height = 14, width = 14, units = "in", dpi = 500)

# Heidegger sub-community
heidegger.net <- get.sub.net(net.directed, "HEIDEGGER M")
heidegger.net$visualization # Network plot object
heidegger.net$hive # Hive plot object
heidegger.top.members <- heidegger.net$nodes.attributes %>%
  group_by(community) %>%
  top_n(5, degree) %>%
  arrange(community, desc(degree)) %>%
  select(name, degree, community)
write_csv(heidegger.top.members, "heidegger_communities.csv")
ggsave(plot = heidegger.net$hive, "heidegger_hive.pdf", height = 14, width = 14, units = "in", dpi = 500)

# Merleauponty sub-community
merleauponty.net <- get.sub.net(net.directed, "MERLEAUPONTY M")
merleauponty.net$visualization # Network plot object
merleauponty.net$hive # Hive plot object
merleauponty.top.members <- merleauponty.net$nodes.attributes %>%
  group_by(community) %>%
  top_n(5, degree) %>%
  arrange(community, desc(degree)) %>%
  select(name, degree, community)
write_csv(merleauponty.top.members, "merleauponty_communities.csv")
ggsave(plot = merleauponty.net$hive, "merleauponty.net_hive.pdf", height = 14, width = 14, units = "in", dpi = 500)


marx.net <- get.sub.net(net.directed, "MARX K")
marx.net$visualization


sartre.net <- get.sub.net(net.directed, "SARTRE J")
sartre.net$visualization

dreyfus.net <- get.sub.net(net.directed, "DREYFUS H")
dreyfus.net$visualization

searle.net <- get.sub.net(net.directed, "SEARLE J", 15) # Don't understand why this one doesn't show labels.
searle.net$visualization
