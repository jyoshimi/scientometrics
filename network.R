library(tidyverse)
library(igraph)
library(ggraph)
library(Matrix)
library(Rtsne)

# Load a cooler/larger palette. Check possibilities of pals package here: http://127.0.0.1:10359/library/pals/doc/pals_examples.html
# This package is used in the ggplot statements.
library(pals)


analyze.network <- function(object, target.author = NULL, 
                            directed = FALSE, threshold = 0.01, 
                            n.top = 10){
  if(is.data.frame(object)){ # Check if the object is already a network or not
    print("Analyzing whole network")
    mode <- "whole"
    network <- igraph::graph_from_data_frame(object, directed = FALSE) %>%
      simplify()
    V(network)$name <- str_to_title(V(network)$name)
  } else if(is.igraph(object) & is.character(target.author)){ # If it's not, function looks for sub communities of the community of target author.
    print(paste("Analyzing the sub network of", target.author))
    mode <- target.author
    author.community <- V(object)$community[which(V(object)$name == target.author)]
    network <- induced.subgraph(object, which(V(object)$community == author.community)) %>%
      as.undirected()
    V(network)$name <- str_to_title(V(network)$name)
  } else {stop("Provide either an edge data frame or an igraph object along with an author in the form Husserl E.")}
  
  # Community detection
  print("Detecting communities")
  net.communities <- cluster_louvain(network)
  
  # Insert graph attributes
  network <- set.vertex.attribute(graph = network, name = "degree",
                                          value = degree(network, mode = "total"))
  network <- set.vertex.attribute(graph = network, name = "strength",
                                          value = strength(network, mode = "total", loops = FALSE))
  network <- set.vertex.attribute(graph = network, name = "community",
                                          value = membership(net.communities))
  
  # Get the data frame of the network vertices for easier analysis
  net.df <- get.data.frame(network, "vertices")
  
  # Get community labels as the top-degree author of every community
  community.labels <- net.df %>%  # Order them by degree within community, then choose the first row of every  community
    arrange(community, desc(degree)) %>% 
    distinct(community, .keep_all = TRUE) %>% 
    .$name %>% 
    str_extract(pattern = "[:alpha:]+(?=[:space:]*)") # Extract everything before a space, or the whole thing if there's no space
  
  # Set new labels in df
  net.df <- net.df %>% 
    mutate(community = factor(community, labels = community.labels))
  
  # Set the labels in the network too
  V(network)$community <- as.character(net.df$community)
  
  # Get the stats per community
  community.stats <- net.df %>% 
    group_by(community) %>% 
    summarize(size = n(), 
              mean.degree = mean(degree), 
              mean.strength = mean(strength),
              total.degree = sum(degree), 
              total.strength = sum(strength)) %>% 
    mutate(relative.size = size / sum(size)) # Proportion of total nodes in this community
  
  # Filter communities with less than {threshold} of the members of the network
  community.stats <- community.stats %>% 
    filter(relative.size > threshold)
  
  # Filter the nodes of those communities from the network and df too
  net.df <- net.df %>% 
    filter(community %in% unique(community.stats$community))
  delete.nodes <- V(network)[!(V(network)$community %in% unique(net.df$community))]
  network <- delete_vertices(network, delete.nodes)
  
  # Get the top members of the community
  top.members <- net.df %>% 
    group_by(community) %>% 
    top_n(n.top, degree) %>% 
    arrange(community)
  
  # Visualization --
  display.labels <- map_chr(V(network)$name, function(name){
    ifelse(name %in% unique(top.members$name), name, NA)
  })
  V(network)$display.label <- display.labels
  
  # davidson harel crashes, gem too
  # fr works well with around 10000 iters
  print("Working on the layout")
  net.layout <- layout_with_fr(network, niter = 10000)
  
  net.viz <- ggraph(network, layout = net.layout) +
    geom_node_point(aes(size = degree, color = community)) + 
    geom_node_label(aes(label = display.label, size = degree, color = community), repel = TRUE) +
    scale_colour_manual(values = as.vector(alphabet(length(unique(net.df$community)))))
  
  result <- list(network.object = network,
                 network.df = net.df,
                 stats = community.stats,
                 net.top = top.members,
                 visualization = net.viz)
  return(result)
}
