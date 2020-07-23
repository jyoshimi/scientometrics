library(tidyverse)
library(igraph)
library(ggraph)
library(Matrix)
# Load a cooler/larger palette. Check possibilities of pals package here: http://127.0.0.1:10359/library/pals/doc/pals_examples.html
# This package is used in the ggplot statements.
library(pals)

# Main functions to use in analyzing citation and co-citation networks

analyze.network <- function(object, target.author = NULL, 
                            directed = FALSE, threshold = 0.01, 
                            n.top = 10, community.algorithm = igraph::cluster_louvain,
                            layout.function = igraph::layout_with_mds, ...){
  # Function for analyzing an edge list OR network object. If provided with an
  # edge list, the network is built during the procedure. If provided with an
  # already constructed network, a sub-network is induced.
  # Can use different community algorithms and layout algorithms. Communities have 
  # the name of its highest (in if directed) degree author  
  # Input:    `object`: either an edge list (as data frame) OR igraph network.
  #             If igraph network, `target.author` is required. Provided igraph needs a
  #             `community` and a `name` attribute to work.
  #           `target.author`:  a string with the name of the author whose
  #           sub-network is to be induced. The analyzed network will then be
  #           the network comprised only of nodes and edges of this author's
  #           community, extracted from the `object`. String must be in Title Case.
  #           `directed`: Logical. Whether the network used for the layout is
  #           directed or not. Community detection is always carried out in an
  #           undirected version of the network.
  #           `threshold`: the CUTOUT FOR COMMUNITY FILTERING. Communities
  #           with fewer than `threshold` (as a proportion) of the number of
  #           nodes in the total network will be deleted along with their
  #           members.
  #           `n.top`: a number with the number of `top members` whose tags will
  #           be plotted and will be listed in the top.members dataframe. These
  #           members are the highest (in if directed) degree authors of the
  #           community.
  #           `community.algorithm`: community detection algorithm to be used.
  #           Needs to be an igraph clustering function. Default is Louvain.
  #           `layout.function`: the layout algorithm for the network. Has to be
  #           an igraph layout function. Default is layout_with_mds.
  #           ...; you can additionally provide NAMED ARGUMENTS for the layout
  #           function as part of the function call, e.g. `niter = 10000` for
  #           layout_with_fr
  # Returns:  A list.
  #           `network.object`: the igraph representation of the network. Can be
  #           used for extracting sub communities.
  #           `network.df`: Information of all nodes including (in) degree and
  #           (in) strength for the network as a dataframe.
  #           `stats`: Information about every community in the network.
  #           Includes mean (in)degree, mean (in)strength, total (in)degree,
  #           total (in)strength and the proportion of total nodes that are in
  #           that community.
  #           `visualization`: ggraph plot of the network.
  
  
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
  
  # Filter communities with less than {argument}`threshold`` of the members of the network
  community.stats <- community.stats %>% 
    filter(relative.size > threshold)
  
  # Filter the nodes and edges of those communities from the network and df too
  net.df <- net.df %>% 
    filter(community %in% unique(community.stats$community))
  delete.nodes <- V(network)[!(V(network)$community %in% unique(net.df$community))]
  network <- delete_vertices(network, delete.nodes)
  
  # Get the top members of the community
  top.members <- net.df %>% 
    group_by(community) %>% 
    top_n(n.top, degree) %>% 
    arrange(community)
  
  # Built network visualization
  # Only display the labels for the top members of each community
  display.labels <- map_chr(V(network)$name, function(name){
    # TODO: The blank string will create a problem for GGPlot
    ifelse(name %in% unique(top.members$name), name, "")
  })
  V(network)$display.label <- display.labels
  
  # Set a layout. Because of the size of the network, a lot of these crash.
  # For smaller networks, MDS works pretty well. For the complete networks,
  # either layout_with_fr with niter = 10000 or layout_with_drl work well.
  print("Working on the layout")
  # Additional arguments for the layout function can be provided in the top
  # function call. They get used here thanks to the ellipsis.
  net.layout <- layout.function(network, ...)
  
  net.viz <- ggraph(network, layout = net.layout) +
    geom_node_point(aes(size = degree, color = community)) + 
    geom_node_label(aes(label = display.label, size = degree, color = community), repel = TRUE) +
    scale_colour_manual(values = as.vector(alphabet(length(unique(net.df$community)))))
  
  # Save all results in a list and return it
  result <- list(network.object = network,
                 network.df = net.df,
                 stats = community.stats,
                 net.top = top.members,
                 visualization = net.viz)
  return(result)
}


write.results <- function(network, network.name){
  # Function to save the results of a call of network.analysis. Saves each of
  # the elements in a different, appropriate format.
  # Community stats and top members are written as csv. Network object is
  # written as graphml to use with Gephi. Visualization is saved both as pdf and
  # png.
  # Input:    `network`: list of results as outputted by network.analysis().
  #           `network.name`: string, name to be appended to the objects written
  #           (e.g. "husserl_citation")
  # Returns:  NULL
  # write stats
  write_csv(x = network$stats, path = paste0("results/", network.name, "_stats", ".csv"))
  
  # write top members
  write_csv(x = network$net.top, path = paste0("results/", network.name, "_tops", ".csv"))
  
  # write gephi
  write_graph(network$network.object, file = paste0("gephi/", network.name, ".graphml"), format = "graphml")
  
  # write plot
  # TODO: Fix display labels; see around line 170 
  # ggsave(plot = network$visualization, paste0("figures/", network.name, ".pdf"), height = 14, width = 14, units = "in", dpi = 500)
  # ggsave(plot = network$visualization, paste0("figures/", network.name, ".png"), height = 14, width = 14, units = "in", dpi = 500)
}



# Purgatory -----

# 
# #From Contreras Kallens & Dale (2018)
# 
# generateCosineMatrix <- function(matrix) {
#   lengthOfVector <- sqrt(rowSums(matrix * matrix))
#   tcrossprod(matrix) / (lengthOfVector %o% lengthOfVector)
# }  #Function that generates the cosine between each row of a matrix.
# 
# # Shrink cocitation matrix to only the names that appear in the reduced network
# 
# cocitation.matrix <- Matrix::readMM('cocitation_matrix.txt') %>%
#   as.matrix()
# row.names(cocitation.matrix) <- read_lines('cocitation_matrix_rowames.txt') %>%
#   str_to_title()
# colnames(cocitation.matrix) <- read_lines('cocitation_matrix_colnames.txt') %>%
#   str_to_title()
# 
# 
# cocitation.matrix.reduced <- cocitation.matrix[V(coc.net$network.object)$name, V(coc.net$network.object)$name]
# 
# cosine.cocitation <- generateCosineMatrix(cocitation.matrix.reduced)
# 
# tsne <- Rtsne(cosine.cocitation, perplexity = 50,
#               pca = FALSE,
#               # partial_pc = TRUE,
#               verbose = TRUE,
#               check_duplicates = FALSE,
#               num_threads = 4)
# 
# tsne.result <- tsne$Y %>% 
#   as_tibble() %>% 
#   add_column(name = row.names(cosine.cocitation)) %>% 
#   mutate(display.label = ifelse(name %in% coc.net$net.top$name, name, NA),
#          community.head = ifelse(name %in% unique(coc.net$stats$community), name, NA))
# tsne.plot <- tsne.result %>%
#   left_join(coc.net$network.df) %>% 
#   mutate(norm.degree = sqrt(degree))
# 
# tsne.plotly <- plotly::plot_ly(data = tsne.plot, x = ~V1, y = ~V2, color = ~community, colors = "Set2",
#                                hovertext = ~paste("Name: ", name, '<br>Community:', community, 
#                                                   "<br>Degree: ", degree),
#                                text = ~community.head,
#                                mode = "markers+text",
#                                type = "scatter",
#                                hoverinfo = "text",
#                                marker = list(size = ~norm.degree, sizeref = 2 * max(tsne.plot$degree) / (80^2),
#                                              sizemode = "area"),
#                                textfont = list(color = "black", size = 16))
# htmlwidgets::saveWidget(tsne.plotly, "tsne_interactive.html")
# 
# tsne.viz <- ggplot(tsne.plot, aes(x = V1, y = V2, label = display.label, 
#                                   color = community, size = degree, 
#                                   text = paste("Name: ", name, '<br>Community:', 
#                                                community, "<br>Degree: ", degree))) + 
#   geom_point() +
#   ggrepel::geom_label_repel() +
#   scale_color_brewer(type = "qual", palette = "Set2")
# 
# tsne.viz
# ggsave("tsne_viz.pdf", scale = 2)
# 
