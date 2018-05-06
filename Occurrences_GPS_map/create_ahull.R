# Create alpha hull function

require(alphahull)
require(igraph)

# function to create an alpha shaped concave hull around spatial points. Requires a dataset
# and an alpha number, determining the concavity of the hull
create_ahull <- function(data, alpha){
  ex_ashape = ashape(data, alpha = alpha)# Computation And Order Alpha Shape
  ex_mat = ex_ashape$edges[, c("ind1", "ind2")]# Take the coordinates of points on the edges of the alpha shape
  class(ex_mat) = "character"# Convert 'numeric' matrix to 'character' matrix, to avoid wrong node orders 
  ex_graph = graph.edgelist(ex_mat, directed = F)# Make the graph of points
  # plot(ex_graph)# Verify its a cyclic graph
  cut_graph = ex_graph - E(ex_graph)[1]  # Cut the first edge
  ends = names(which(degree(cut_graph) == 1))   # Get two nodes with degree = 1
  path = get.shortest.paths(cut_graph, ends[1], ends[2])$vpath[[1]]# Compute a path
  path_nodes = as.numeric(V(ex_graph)[path]$name)# Get node names (= row numbers in original data.frame)
  return(path_nodes)
}