library(tidyverse)
library(igraph)
library(ggraph)
library(Matrix)
library(Rtsne)

# Load a cooler/larger palette. Check possibilities of pals package here: http://127.0.0.1:10359/library/pals/doc/pals_examples.html
# This package is used in the ggplot statements.
library(pals)


# Load data ----

# edge list
edge.list <- read_csv('cocitation_edge_list.csv') %>%
  rename(weight = Weight) %>% 
  filter(Source != Target) # To remove self-citations
cocitation.matrix <- Matrix::readMM('cocitation_matrix.txt') %>% 
  as.matrix()
row.names(cocitation.matrix) <- read_lines('cocitation_matrix_rowames.txt') %>% 
  str_to_title()
colnames(cocitation.matrix) <- read_lines('cocitation_matrix_colnames.txt') %>% 
  str_to_title()


analyze.network <- function(object, target.author = NULL, 
                            directed = FALSE, threshold = 0.01, 
                            n.top = 10){
  if(is.data.frame(object)){ # Check if the object is already a network or not
    print("Analyzing whole network")
    mode <- "whole"
    network <- igraph::graph_from_data_frame(edge.list, directed = FALSE) %>%
      simplify()
    V(network)$name <- str_to_title(V(network)$name)
  } else if(is.igraph(object) & is.character(target.author)){ # If it's not, function looks for sub communities of the community of target author.
    print(paste("Analyzing the sub network of", target.author))
    mode <- target.author
    author.community <- V(object)$community[which(V(object)$name == "Husserl E")]
    network <- induced.subgraph(object, which(V(object)$community == author.community)) %>%
      as.undirected()
    V(network)$name <- str_to_title(V(network)$name)
  }
  
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
  net.layout <- layout_with_mds(network)
  
  net.viz <- ggraph(network, layout = net.layout) +
    geom_node_point(aes(size = degree, color = community)) + 
    geom_node_label(aes(label = display.label, size = degree, color = community), repel = TRUE) +
    scale_colour_manual(values = as.vector(alphabet(length(unique(nodes$community)))))
  
  result <- list(network.object = network,
                 network.df = net.df,
                 stats = community.stats,
                 net.top = top.members,
                 visualization = net.viz)
  return(result)
}

whole.network <- analyze.network(edge.list)

husserl.network <- analyze.network(whole.network$network.object, target.author = "Husserl E")
husserl.network$visualization


# Network -----
coc.net <-  igraph::graph_from_data_frame(edge.list, directed = FALSE) %>%
  simplify()
V(coc.net)$name <- str_to_title(V(coc.net)$name)

# Run chosen community detection algorithm
# * Must change depending on which detection alg is used
net.communities <- igraph::cluster_louvain(coc.net)
coc.net <- igraph::set.vertex.attribute(graph = coc.net, name = "degree",
                                             value = degree(coc.net, mode = "total"))
coc.net <- igraph::set.vertex.attribute(graph = coc.net, name = "strength",
                                             value = strength(coc.net, mode = "total", loops = FALSE))
coc.net <- igraph::set.vertex.attribute(graph = coc.net, name = "community",
                                             value = igraph::membership(net.communities))
# Transform to data frame
net.df <- get.data.frame(coc.net, "vertices") %>% 
  mutate(name = str_to_title(name))

# Get community labels
community.labels <- net.df %>% 
  group_by(community) %>% 
  top_n(1, degree) %>% 
  distinct(community, .keep_all = TRUE) %>% 
  arrange(community) %>% 
  .$name 

# Set labels in df
net.df <- net.df %>% 
  mutate(community = factor(community, labels = community.labels))

# Set the labels in the network too
V(coc.net)$community <- as.character(net.df$community)

# Get community stats
community.stats <- net.df %>% 
  group_by(community) %>% 
  summarize(size = n(), mean.degree = mean(degree), mean.strength = mean(strength),
            total.degree = sum(degree), total.strength = sum(strength)) %>% 
  mutate(relative.size = size / sum(size))

# Filter communities with less than 1% of the members of the network
community.stats <- community.stats %>% 
  filter(relative.size > 0.01)

# Now filter the dataframe
net.df <- net.df %>% 
  filter(community %in% unique(community.stats$community))

# Filter the network too
delete.nodes <- V(coc.net)[!(V(coc.net)$community %in% unique(net.df$community))]
coc.net <- delete_vertices(coc.net, delete.nodes)

# Get the top members of the community
top.members <- net.df %>% 
  group_by(community) %>% 
  top_n(10, degree) %>% 
  arrange(community)

display.labels <- map_chr(V(coc.net)$name, function(name){
  ifelse(name %in% unique(top.members$name), name, NA)
})
V(coc.net)$display.label <- display.labels

# Get layout with tsne


# TSNE STUFF ------

#From Contreras Kallens & Dale (2018)

generateCosineMatrix <- function(matrix) {
  lengthOfVector <- sqrt(rowSums(matrix * matrix))
  tcrossprod(matrix) / (lengthOfVector %o% lengthOfVector)
}  #Function that generates the cosine between each row of a matrix.

# Shrink cocitation matrix to only the names that appear in the reduced network

cocitation.matrix.reduced <- cocitation.matrix[V(coc.net)$name, V(coc.net)$name]

cosine.cocitation <- generateCosineMatrix(cocitation.matrix.reduced)

tsne <- Rtsne(cosine.cocitation, perplexity = 50,
              pca = FALSE,
              # partial_pc = TRUE,
              verbose = TRUE,
              check_duplicates = FALSE,
              num_threads = 4)

tsne.result <- tsne$Y %>% 
  as_tibble() %>% 
  add_column(name = row.names(cosine.cocitation)) %>% 
  mutate(display.label = ifelse(name %in% top.members$name, name, NA),
         community.head = ifelse(name %in% community.labels, name, NA))
tsne.plot <- tsne.result %>%
  left_join(net.df) %>% 
  mutate(norm.degree = sqrt(degree))

tsne.plotly <- plotly::plot_ly(data = tsne.plot, x = ~V1, y = ~V2, color = ~community, colors = "Set2",
                hovertext = ~paste("Name: ", name, '<br>Community:', community, 
                               "<br>Degree: ", degree),
                text = ~community.head,
                mode = "markers+text",
                type = "scatter",
                hoverinfo = "text",
                marker = list(size = ~norm.degree, sizeref = 2 * max(tsne.plot$degree) / (80^2),
                              sizemode = "area"),
                textfont = list(color = "black", size = 16))
htmlwidgets::saveWidget(tsne.plotly, "tsne_interactive.html")

tsne.viz <- ggplot(tsne.plot, aes(x = V1, y = V2, label = display.label, 
                       color = community, size = degree, 
                       text = paste("Name: ", name, '<br>Community:', 
                                    community, "<br>Degree: ", degree))) + 
  geom_point() +
  ggrepel::geom_label_repel() +
  scale_color_brewer(type = "qual", palette = "Set2")

tsne.viz
ggsave("tsne_viz.pdf", scale = 2)


# With net layout


net.layout <- layout_with_mds(coc.net)
# davidson harel crashes, gem too
# fr works well with around 10000 iters.
net.viz <- ggraph(coc.net, layout = net.layout) +
  geom_node_point(aes(size = degree, color = community)) + 
  geom_node_label(aes(label = display.label, size = degree, color = community), repel = TRUE)

net.viz



+
  scale_colour_manual(values = as.vector(alphabet(length(unique(nodes$community))))) # Generates a palette with number of colors = number of communities. as.vector needed because ggplot doesn't like colors to have names.








tidynet <- tidygraph::as_tbl_graph(coc.net)

# Get nodes as a dataframe
nodes <- tidynet %>%
  tidygraph::activate(nodes) %>%
  as_tibble()
net.layout <- igraph::layout.drl(coc.net, options = drl_defaults$default)
# Create display labels for gephi
display.labels <- igraph::vertex.attributes(coc.net)[[1]] %>% 
  map_chr(function(name){
    name <- stringr::str_to_title(name)
    if(name %in% top.per.cluster$name){
      return(name)
    } else {return("")}
  })
coc.net <- igraph::set.vertex.attribute(graph = coc.net, name = "display_label",
                                             value = display.labels)
tidynet <- tidygraph::as_tbl_graph(coc.net) %>% 
  activate(nodes) %>% 
  mutate(community = factor(community))

# Plot network using colors for communities, repel the labels so they don't overlap, and don't plot the links. **
# This uses the modified weights to exaggerate the structure.
net.viz <- ggraph(tidynet, layout = net.layout) +
  geom_node_point(aes(size = degree, color = community)) +
  geom_node_label(aes(label = display_label, size = degree, color = community), repel = TRUE) +
  scale_colour_manual(values = as.vector(alphabet(length(unique(nodes$community))))) # Generates a palette with number of colors = number of communities. as.vector needed because ggplot doesn't like colors to have names.

# Quick view
net.viz
