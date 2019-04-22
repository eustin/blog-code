
get_path_length <- function(path, osm_graph) {
  require(itertools)
  
  adjacency_matrix <- create_weighted_adjacency_matrix(osm_graph)
  edge_iterator <- create_edge_iterator(path)
  edge_weights <- sapply(edge_iterator, edge_weight, adjacency_matrix)
  total_distance_in_kms <- sum(edge_weights) / 1000
  return(total_distance_in_kms)
}


create_weighted_adjacency_matrix <- function(osm_graph) {
  return(as_adjacency_matrix(osm_graph, attr = 'weight'))
}


create_edge_iterator <- function(path) {
  path_len <- length(path)
  first_node <- path[1:(path_len - 1)]
  second_node <- path[2:path_len]
  result <- izip(first_node, second_node)
  return(result)
}


edge_weight <- function(edge, adjacency_matrix) {
  node_from <- edge[[1]]
  node_to <- edge[[2]]
  row_idx <- match(node_from, rownames(adjacency_matrix))
  col_idx <- match(node_to, colnames(adjacency_matrix))
  weight <- adjacency_matrix[row_idx, col_idx]
  return(weight)
}

