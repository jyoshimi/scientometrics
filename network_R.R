library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
# Load a cooler/larger palette. Check possibilities of pals package here: http://127.0.0.1:10359/library/pals/doc/pals_examples.html
# This package is used in the ggplot statements.
library(pals)

# Things to fiddle with marked with *

# Preliminary ------

# Meaningful structure from fastgreedy.community, louvain, walktrap, and spinglass
# All others return few commnities or take a long time to run
detect.community <- igraph::cluster_louvain

# Change this to where the files are stored.
# Choices here are small_edge_list.csv and complete_edge_list.csv
edge.list <- read_csv('complete_edge_list.csv') %>%
  rename(weight = Weight) %>% 
  filter(Source != Target) # To remove self-citations

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

# Add indegree, WEIGHTED in-degree (a.k.a. strength) and 
# community as vertex attributes to the igraph network.
# Used  for plotting.
# Added as attributes to the directed igraph
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "degree",
                                             value = degree(net.directed, mode = "in"))
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "strength",
                                             value = strength(net.directed, mode = "in", loops = FALSE))
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "community",
                                             value = igraph::membership(net.communities))
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "outdegree",
                                             value = degree(net.directed, mode = "out"))
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "betweenness",
                                             value = betweenness(net.directed, normalized = TRUE))

# Explore membership ------

# Import as tidynet, which represents graph as a dataframe and it's simpler to visualize it
# This is used for all graphing
tidynet <- tidygraph::as_tbl_graph(net.directed)

# Get nodes as a dataframe
# Useful to view
nodes <- tidynet %>%
  tidygraph::activate(nodes) %>%
  as_tibble()

  # Some Code snippets and Stats ------

# Get top N authors by indegree (no communities)
tops <- nodes %>% 
  top_n(10,degree) %>% 
  mutate(name = str_to_title(name)) %>% 
  arrange(desc(degree))
write_csv(tops,"tops.csv")

# Uncomment to save list of top in-degree authors in each cluster
top.members <- nodes %>%
  group_by(community) %>%
  top_n(10, degree) %>%
  arrange(community, desc(degree)) %>%
  # select(name, degree, community) %>% 
  mutate(name = str_to_title(name))
write_csv(top.members, "top_authors_by_community.csv") # uncomment this line to save result to csv

# Complete list of all authors, ordered bycommunity memberships
all.members <- nodes %>%
  arrange(community, desc(strength)) %>%
  # select(name, strength, community)  %>% 
  mutate(name = str_to_title(name))
write_csv(all.members, "all_authors_by_community.csv") # uncomment this line to save result to csv

# Get community degree stats: sum in-degree, mean-indegree, mean eigen
# Especially useful for Gephi
com.stats <- nodes %>% 
  group_by(community) %>% 
  summarize(sum.degree = sum(degree), 
            mean.degree = mean(degree), 
            sum.strength = sum(strength),
            mean.strength = mean(strength),
            mean.eigen = median(scale(eigen)))
write_csv(com.stats, "community_stats.csv") # uncomment this line to save result to csv

# Num authors overall:
# nrow(nodes)
# Number who self-cite
# edge.list %>% filter(Source == Target) %>% nrow 


# Get citing authors
all.citing <- nodes %>%
  filter(outdegree > 0) %>%
  group_by(community) %>%
  top_n(20, outdegree)  %>%
  arrange(community, desc(outdegree)) %>%
  # select(name, strength, community)  %>%
  mutate(name = str_to_title(name))
nrow(all.citing)
all.citing %>% group_by(community) %>% tally()
View(all.citing)

# Get exclusively cited authors
# all.cited <- nodes %>%
#   arrange(community, desc(strength)) %>%
#   # select(name, strength, community)  %>%
#   mutate(name = str_to_title(name)) %>%
#   filter(outdegree == 0)
# nrow(all.cited)

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
# NOTE: I DIDNT USE THIS FOR THE FORCE ATLAS 2 LAYOUT

edge.weights <- function(community, network, weight.within = 1.5, weight.between = 1) {
  bridges <- crossing(communities = community, graph = network)
  weights <- ifelse(test = bridges, yes = weight.between, no = weight.within)
  return(weights)
}
# change below to directed or undirected *
group.edges <- edge.weights(net.communities, net.directed)

# Set weighted edges in igraph object for exporting

layout.weights <- igraph::edge.attributes(net.directed)[[1]] * group.edges
net.directed <- igraph::set.edge.attribute(graph = net.directed, name = "weight",
                                             value = layout.weights)
# Create display labels for gephi

top.labels <- igraph::vertex.attributes(net.directed)[[1]] %>% 
  map_chr(function(name){
    name <- stringr::str_to_title(name)
    if(name %in% top.members$name){
      return(name)
      } else {return("")}
  })
net.directed <- igraph::set.vertex.attribute(graph = net.directed, name = "display_label",
                                             value = top.labels)


# SAVE THE GRAPH in GRAPHML format.
# This format can be read by GEPHI.
net.directed %>% 
  write_graph(file = "gephi_test.graphml", format = "graphml")

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
  scale_colour_manual(values = as.vector(alphabet(length(unique(nodes$community))))) # Generates a palette with number of colors = number of communities. as.vector needed because ggplot doesn't like colors to have names.

# Quick view
net.viz

 # Uncomment line to save network viz as a pdf.  Can play with resolution and size. *
ggsave(plot = net.viz, "citation_network.pdf", height = 14, width = 14, units = "in", dpi = 500)

#
# Hive stuff------
#

# This hacky function generates a field for each edge that's 0 if it's a within-community edge. 
# Useful (I think??) for the hive plot. Probably a better way of doing this.
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
  filter(degree > 25) %>% 
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
  scale_color_manual(values = as.vector(alphabet(length(unique(community.vector))))) +
  scale_edge_color_manual(values = as.vector(alphabet(length(unique(community.vector)))), limits = levels(community.vector))

# Quick view
hive.plot

# Uncomment line to save network viz as a pdf.  Can play with resolution and size. *
ggsave(plot = hive.plot, "hive_plot.pdf", height = 14, width = 14, units = "in", dpi = 500)


# Visualizing sub networks ------

# Function to extract sub network info
get.sub.net <- function(net, author){
  # Function for recursively analyzing a subnet of a network. The procedure is
  # based on the name of an author. It uses the community structure of the input
  # net to detect a community structure within that sub network using the same
  # algorithm set at the beginning of the code.

  # Input:    `net` is a directed igraph object with added vertex attributes for community.
  #           `author` is a string with the name of the target author in
  #             the form LASTNAME INITIAL e.g. `HUSSERL E`
  #           `label_threshold` is the threshold of in-degree value at which labels are
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
  author.clusters <- detect.community(author.net.undirected)

  author.net.directed <- igraph::set.vertex.attribute(graph = author.net.directed, name = "community",
                                                      value = igraph::membership(author.clusters))
  author.net.directed <- igraph::set.vertex.attribute(graph = author.net.directed, name = "degree", value = degree(author.net.directed, mode = "in"))

  author.top.members <- tidygraph::as_tbl_graph(author.net.directed) %>% 
    tidygraph::activate(nodes) %>%
    as_tibble() %>% 
    group_by(community) %>%
    top_n(10, degree) %>%
    arrange(community, desc(degree)) %>%
    mutate(name = str_to_title(name))
  print(author.top.members)
  author.top.labels <- igraph::vertex.attributes(author.net.directed)[[1]] %>% 
    map_chr(function(name){
      name <- stringr::str_to_title(name)
      if(name %in% author.top.members$name){
        return(name)
      } else {return("")}
    })
  print(author.top.labels)
  
  author.net.directed <- igraph::set.vertex.attribute(graph = author.net.directed, name = "display.name",
                                               value = author.top.labels)
  
  tidy.author.net <- tidygraph::as_tbl_graph(author.net.directed)
  
  author.nodes <- tidy.author.net %>% 
    activate(nodes) %>% 
    as_tibble
  
  author.com.stats <- author.nodes %>% 
    group_by(community) %>% 
    summarize(sum.degree = sum(degree), 
              mean.degree = mean(degree), 
              sum.strength = sum(strength),
              mean.strength = mean(strength))
  tidy.author.net <- tidy.author.net %>%
    activate(edges) %>%
    mutate(inverse.weight = (E(author.net.directed)$weight * opposite.weights(author.clusters, author.net.directed)) ^ 2)
 
  author.group.edges <- edge.weights(author.clusters, author.net.directed)

  author.net.layout <- igraph::layout.drl(author.net.directed, options = drl_defaults$default, weights = (E(author.net.directed)$weight * author.group.edges))

  author.viz <- ggraph(tidy.author.net, layout = author.net.layout) +
    geom_node_point(aes(size = degree, color = community)) +
    geom_node_label(aes(label = display.name, size = degree, color = community), repel = TRUE) +
    scale_colour_manual(values = as.vector(alphabet(length(unique(author.nodes$community)))))

  small.tidy.author <- tidy.author.net %>%
    activate(nodes) %>%
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
    scale_color_manual(values = as.vector(alphabet(length(unique(author.community.vector))))) +
    scale_edge_color_manual(values = as.vector(alphabet(length(unique(author.community.vector)))), limits = levels(author.community.vector))

  return(list(igraph.network = author.net.directed,
              nodes.attributes = author.nodes,
              top.members.comm = author.top.members,
              stats = com.stats,
              visualization = author.viz,
              hive = author.hive))
}

# Manually use the get.sub.net function for communities of interest

# Husserl sub-community------
husserl.net <- get.sub.net(net.directed, "HUSSERL E")

# Graphs
husserl.net$hive # Hive plot object
husserl.net$visualization # Network plot object
# ggsave(plot = husserl.net$hive, "husserl_hive.pdf", height = 14, width = 14, units = "in", dpi = 500)

# Top members
husserl.top.members <- husserl.net$top.members.comm
write_csv(husserl.top.members, "husserl_communities.csv")
# Number of exclusively cited authors:sum(husserl.top.members$outdegree == 0)  
# Number of citing authors: sum(husserl.top.members$outdegree > 0)  
# Number of authors in "Husserl Cluster" who don't cite Husserl: 111
#  sum(husserl.top.members$outdegree > 0) - husserl.top.members$degree[1]

# Stats
# husserl.net$stats
# write_csv(husserl.net$stats, "husserl_stats.csv") # uncomment this line to save result to csv


# Heidegger sub-community------
heidegger.net <- get.sub.net(net.directed, "HEIDEGGER M", label_threshold = 10)
heidegger.net$visualization # Network plot object
heidegger.net$hive # Hive plot object
heidegger.top.members <- heidegger.net$nodes.attributes %>%
  group_by(community) %>%
  top_n(20, degree) %>%
  arrange(community, desc(degree)) %>%
  select(name, degree, community)
write_csv(heidegger.top.members, "heidegger_communities.csv")
# ggsave(plot = heidegger.net$hive, "heidegger_hive.pdf", height = 14, width = 14, units = "in", dpi = 500)
heidegger.net.stats <- heidegger.net$nodes %>% 
  group_by(community) %>% 
  summarize(sum.degree = sum(degree), 
            mean.degree = round(mean(degree), digits = 3), 
            sum.strength = sum(strength),
            mean.strength = round(mean(strength), digits=3)) %>% 
  arrange(desc(sum.degree))
write_csv(heidegger.net.stats, "heidegger_stats.csv") # uncomment this line to save result to csv

# Merleauponty sub-community------
merleauponty.net <- get.sub.net(net.directed, "MERLEAUPONTY M", label_threshold = 20)
merleauponty.net$visualization # Network plot object
merleauponty.net$hive # Hive plot object
merleauponty.top.members <- merleauponty.net$nodes.attributes %>%
  group_by(community) %>%
  top_n(20, degree) %>%
  arrange(community, desc(degree)) %>%
  select(name, degree, community)
write_csv(merleauponty.top.members, "mp_communities.csv")
# ggsave(plot = merleauponty.net$hive, "merleauponty.net_hive.pdf", height = 14, width = 14, units = "in", dpi = 500)
merleauponty.net.stats <- merleauponty.net$nodes %>% 
  group_by(community) %>% 
  summarize(sum.degree = sum(degree), 
            mean.degree = round(mean(degree), digits = 3), 
            sum.strength = sum(strength),
            mean.strength = round(mean(strength), digits=3)) %>% 
  arrange(desc(sum.degree))
write_csv(merleauponty.net.stats, "mp_stats.csv") # uncomment this line to save result to csv


# Mark and others------
marx.net <- get.sub.net(net.directed, "MARX K")
marx.net$visualization
marx.net$hive


sartre.net <- get.sub.net(net.directed, "SARTRE J", label_threshold = 20)
sartre.net$visualization

dreyfus.net <- get.sub.net(net.directed, "DREYFUS H", label_threshold = 20)
dreyfus.net$visualization

searle.net <- get.sub.net(net.directed, "SEARLE J", label_threshold = 20) # Don't understand why this one doesn't show labels.
searle.net$visualization
searle.net$hive
