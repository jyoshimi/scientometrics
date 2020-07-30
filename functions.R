library(tidyverse)
library(igraph)
library(ggraph)
library(Matrix)
# Load a cooler/larger palette. Check possibilities of pals package here: http://127.0.0.1:10359/library/pals/doc/pals_examples.html
# This package is used in the ggplot statements.
library(pals)

# Functions to use in analyzing citation and co-citation networks

build.network <- function(object, target.author = NULL, directed = FALSE, 
                          community.algorithm = igraph::cluster_louvain,
                          threshold = 0.01,
                          num.to.display = 5){
    
    # Main function to buld network objects
    #
    # If provided with an  edge list, the network is built during the procedure.
    # If provided with an already constructed network, a sub-network is induced.
    # Can use different community algorithms and layout algorithms. Communities have 
    # the name of its highest (in if directed) degree author  
    #
    # Input:    `object`: either an edge list (as data frame) or igraph network.
    #             If igraph network, `target.author` is required. 
    #
    #           `target.author`:  a string with the name of the author whose
    #           sub-network is to be induced. The analyzed network will then be
    #           the network comprised only of nodes and edges of this author's
    #           community, extracted from the `object`. String must be in Title Case.
    #
    #           `directed`: Logical. Whether the network used for the layout is
    #           directed or not. Community detection is always carried out in an
    #           undirected version of the network.
    #
    #           `threshold`: Communities with fewer than `threshold` (as a proportion)
    #           of the number of nodes in the total network will be deleted along 
    #            with their  members.
    #
    #           num.to.display: a number with the number of `top members` whose tags will
    #           be plotted and will be listed in the top.members dataframe. These
    #           members are the highest (in if directed) degree authors of the
    #           community.
    #
    #           `community.algorithm`: community detection algorithm to be used.
    #           Needs to be an igraph clustering function. Default is Louvain.
    #
    # Returns:  A list.
    #           `network.object`: the igraph representation of the network. Can be
    #           used for extracting sub communities.
    #
    #           `network.df`: Information of all nodes including (in) degree and
    #           (in) strength for the network as a dataframe.
    #
    #           `stats`: Information about every community in the network.
    #           Includes mean (in)degree, mean (in)strength, total (in)degree,
    #           total (in)strength and the proportion of total nodes that are in
    #           that community.
  
  
  # Check if the object is already a network or not. If it is, set function mode as "whole"
  if(is.data.frame(object)){ 
    print("Analyzing whole network")
    mode <- "whole"
    
    network <- igraph::graph_from_data_frame(object, directed = FALSE) %>%
      simplify()
    V(network)$name <- str_to_title(V(network)$name)
    # If it's directed, save a second temporary directed network
    if(directed){
      network.directed <- igraph::graph_from_data_frame(object, directed = TRUE) %>%
        simplify()
      V(network.directed)$name <- str_to_title(V(network.directed)$name)
      
    }
    
    # If it's not, a network, the function induces a sub-community. Thus, first
    # check is whether the provided object is an igraph network AND if a target
    # author was provided. Set function mode as target author
  } else if(is.igraph(object) & is.character(target.author)){ 
    print(paste("Analyzing the sub network of", target.author))
    mode <- target.author
    # Extract the community of the target author
    author.community <- V(object)$community[which(V(object)$name == target.author)]
    # Obtain the target author's community nodes and edges
    network <- induced.subgraph(object, which(V(object)$community == author.community)) %>%
      as.undirected()
    V(network)$name <- str_to_title(V(network)$name)
    # For a directed sub graph, the provided network has to be directed in addition to directed being set as true.
    if(directed & is.directed(object)){
      network.directed <- induced.subgraph(object, which(V(object)$community == author.community))
      V(network.directed)$name <- str_to_title(V(network.directed)$name)
    } else if(directed & !is.directed(object)){
      stop("Can't induce a directed subnetwork from an undirected network")
    }
  } else {
    stop("Provide either an edge data frame or an igraph object along with an author in the form `Husserl E`")
  }
  
  # Community detection
  print("Detecting communities")
  net.communities <- community.algorithm(network)
  
  # If directed analysis, overwrite network with its directed counterpart.
  # As communities were saved in prior step, undirected network is not needed anymore.
  if(directed){network <- network.directed}
  
  # Insert graph attributes
  # If network is directed, use indegree and instrength. If not, just degree and
  # strength.
  if(directed){
    network <- set.vertex.attribute(graph = network, name = "degree",
                                    value = degree(network, mode = "in"))
    network <- set.vertex.attribute(graph = network, name = "strength",
                                    value = strength(network, mode = "in", loops = FALSE))
  } else {
    network <- set.vertex.attribute(graph = network, name = "degree",
                                    value = degree(network, mode = "total"))
    network <- set.vertex.attribute(graph = network, name = "strength",
                                    value = strength(network, mode = "total", loops = FALSE))
  }
  
  # Set community as an attribute
  network <- set.vertex.attribute(graph = network, name = "community",
                                  value = membership(net.communities))
  
  # Get the data frame of the network vertices for easier analysis
  net.df <- get.data.frame(network, "vertices")
  
  # Get community names as the top-degree author of every community
  community.labels <- net.df %>%  # Order them by degree within community, then choose the first row of every  community
    arrange(community, desc(degree)) %>% 
    distinct(community, .keep_all = TRUE) %>% 
    .$name %>% 
    # Extract everything before a space, or the whole thing if there's no space.
    # This results in community names like Husserl instead of Husserl E
    str_extract(pattern = "[:alpha:]+(?=[:space:]*)") 
  
  # Set new labels in node data frame
  net.df <- net.df %>% 
    mutate(community = factor(community, labels = community.labels))
  
  # Set the labels in the network too
  V(network)$community <- as.character(net.df$community)
  
  # Get the stats per community
  # Use mean (in)degree, mean (in)strength, the sum of both statistics, and the
  # proportion of total nodes that are in that network.
  community.stats <- net.df %>% 
    group_by(community) %>% 
    summarize(size = n(), 
              mean.degree = mean(degree), 
              mean.strength = mean(strength),
              total.degree = sum(degree), 
              total.strength = sum(strength)) %>% 
    mutate(relative.size = size / sum(size)) # Proportion of total nodes in this community
    
  # Filter communities with less than `threshold`` of the members of the network
  community.stats <- community.stats %>% 
    filter(relative.size > threshold) %>% 
    arrange(desc(total.degree))
  
  # Filter the nodes and edges of those communities from the network and df too
  net.df <- net.df %>% 
    filter(community %in% unique(community.stats$community))
  delete.nodes <- V(network)[!(V(network)$community %in% unique(net.df$community))]
  network <- delete_vertices(network, delete.nodes)
  
  # Get the top members of the community
  top.members <- net.df %>% 
    group_by(community) %>% 
    top_n(num.to.display, degree) %>% 
    arrange(community) 
  
  # Set up display labels for network visualization
  # Only display the labels for the top members of each community
  display.labels <- map_chr(V(network)$name, function(name){
    ifelse(name %in% unique(top.members$name), name, "")
  })
  V(network)$display.label <- display.labels
  
  result <- list(network.object = network,
                 df = net.df,
                 stats = community.stats)
  
  return(result)
}


write.gephi <- function(network, network.name){
  write_graph(network$network.object, file = paste0("gephi/", network.name, ".graphml"), format = "graphml")
}

write.stats <- function(network, network.name){
  write_csv(x = network$stats, path = paste0("results/", network.name, "_stats", ".csv"))
}

write.tops <- function(network, network.name, n.top=10){
  
  # Get the top members of the community
  tops <- network$df %>%
    select(name,degree,strength,community) %>% 
    group_by(community) %>%
    top_n(n.top, degree) %>%
    arrange(community, desc(degree)) 
  
  # write top members
  write_csv(x = tops, path = paste0("results/", network.name, "_tops", ".csv"))
  
}

  
  