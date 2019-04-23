
make_weighted_adj_list <- function(igraph_obj, edge_attr='weight') {
  
  print('creating weighted adjacency list')
  
  # get the nodes at either end of each edge 
  edge_list <- as.data.table(as_edgelist(igraph_obj))
  names(edge_list) <- c('from', 'to')
  # get the distance in metres associated with these edges
  edge_list[, weight:=E(igraph_obj)$weight]
  
  # remove some duplicated edges - e.g. 20827842
  edge_list <- unique(edge_list)
  
  # create hashed environments for fast subsetting 
  nodes_split_from <- split(edge_list, edge_list$from)
  nodes_split_to <- split(edge_list, edge_list$to)
  node_from_env <- list2env(nodes_split_from, hash=TRUE)
  node_to_env <- list2env(nodes_split_to, hash=TRUE)
  
  # create preallocated list for weighted adj list we will be creating
  # we will be using our node IDs as indices of this list
  weighted_adj_list <- vector('list', vcount(igraph_obj))
  names(weighted_adj_list) <- names(V(igraph_obj))

  # process each node ID in our list
  for (node in names(weighted_adj_list)) {
    
    edge_subset <- node_from_env[[node]]
    if (!is.null(edge_subset)) {
      weights <- edge_subset$weight
      names(weights) <- edge_subset$to
      weighted_adj_list[[node]] <- weights
    }
    
    edge_subset <- node_to_env[[node]]
    if (!is.null(edge_subset)) {
      already_appened <- names(weighted_adj_list[[node]])
      not_already_appened <- !edge_subset$from %in% already_appened

      if (length(not_already_appened) == 0) next
      weights <- edge_subset$weight[not_already_appened]
      names(weights) <- edge_subset$from[not_already_appened]
      weighted_adj_list[[node]] <- append(weighted_adj_list[[node]], weights)
    } 
  }
  print('done!')
  return(weighted_adj_list)
}
