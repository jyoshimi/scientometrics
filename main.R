# - Main script -----

source('network.R')

# Load data ----









# Citation ------








# Cocitation ---------



# edge list


co.citation.edges <- read_csv('cocitation_edge_list.csv') %>%
  rename(weight = Weight) %>% 
  filter(Source != Target) # To remove self-citations
# cocitation.matrix <- Matrix::readMM('cocitation_matrix.txt') %>% 
#   as.matrix()
# row.names(cocitation.matrix) <- read_lines('cocitation_matrix_rowames.txt') %>% 
#   str_to_title()
# colnames(cocitation.matrix) <- read_lines('cocitation_matrix_colnames.txt') %>% 
#   str_to_title()

coc.net <- analyze.network(object = co.citation.edges)
zahavi.net <- analyze.network(object = coc.net$network.object, target.author = "Zahavi D")
zahavi.net$visualization






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

