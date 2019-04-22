# this function can be found in this folder as well!
source('get_path_length.R')

# constants
LARGE_NUMBER <- 999999

# osm_graph <- igraph_highways_and_nodes
# source_node <- nearest_to_newtown
# destination_node <- nearest_to_small_bar

dijkstra_shortest_path <- function(osm_graph, source_node, destination_node) {
  
  weighted_adj_list <- make_weighted_adj_list(osm_graph)
  source_node_id <- get_node_id(source_node)
  destination_node_id <- get_node_id(destination_node)
  
  vec_of_distances <- create_distances_vec(osm_graph, source_node_id)
  vec_of_predecessors <- create_predecessors_vec(osm_graph)
  vec_of_nodes <- names(V(osm_graph))
  
  print('dijkstra time!')
  while (nodes_left_to_process(vec_of_nodes)) {
    
    num_nodes_left <- sum(!is.na(vec_of_nodes))
    if (num_nodes_left %% 1000 ==0) {
      print(num_nodes_left)
    }
    
    # get min dist of those nodes left to process
    min_dist_node <- find_min_dist_node(vec_of_nodes, vec_of_distances)
    min_dist_node_id <- min_dist_node[['node_id']]
    
    current_distance_to <- min_dist_node[['current_distance_to']]
    adjacent_to_min_dist_node <- get_adjacent_nodes_left_to_process(weighted_adj_list, vec_of_nodes, min_dist_node_id)
    
    for (i in seq_along(adjacent_to_min_dist_node)) {
      adjacent_node_id <- names(adjacent_to_min_dist_node[i])
      distance_to_adj_node <- adjacent_to_min_dist_node[[i]]
      curr_known_dist_to_adj_node <- get_current_dist_to_adj_node(vec_of_distances, adjacent_node_id)
      
      if (current_distance_to + distance_to_adj_node < curr_known_dist_to_adj_node) {
        vec_of_distances[[adjacent_node_id]] <- current_distance_to + distance_to_adj_node
        vec_of_predecessors[[adjacent_node_id]] = min_dist_node_id
      }
    }
    vec_of_nodes <- remove_min_dist_node(vec_of_nodes, min_dist_node_id)
  }
  
  print('done!')
  print('recovering shortest path from source to destination...')
  shortest_path <- recover_shortest_path(source_node_id, destination_node_id, vec_of_predecessors, vec_of_distances)
  
  results <- list('path' = shortest_path$path, 'distance' = shortest_path$distance, 
                  'distances' = vec_of_distances, 'predecessors' = vec_of_predecessors)
  return(results)
}

make_weighted_adj_list <- function(igraph_obj, edge_attr='weight') {
  
  print('creating weighted adjacency list')
  edge_list <- as.data.table(as_edgelist(igraph_obj))
  names(edge_list) <- c('from', 'to')
  edge_list[, weight:=E(igraph_obj)$weight]
  
  # some duplicated edges - e.g. 20827842
  edge_list <- unique(edge_list)
  
  # create hashed environments for fast subsetting 
  nodes_split_from <- split(edge_list, edge_list$from)
  nodes_split_to <- split(edge_list, edge_list$to)
  
  node_from_env <- list2env(nodes_split_from, hash=TRUE)
  node_to_env <- list2env(nodes_split_to, hash=TRUE)
  
  # create preallocated list for weighted adj list we will be creating
  weighted_adj_list <- vector('list', vcount(igraph_obj))
  names(weighted_adj_list) <- names(V(igraph_obj))

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
  return(weighted_adj_list)
}

remove_min_dist_node <- function(vec_of_nodes, min_dist_node_id) {
  min_dist_node_idx <- which(vec_of_nodes == min_dist_node_id)
  vec_of_nodes[min_dist_node_idx] <- NA_character_
  return(vec_of_nodes)
}

get_current_dist_to_adj_node <- function(vec_of_distances, adjacent_node_id) {
  return(vec_of_distances[[adjacent_node_id]])  
}

get_adjacent_nodes_left_to_process <- function(weighted_adj_list, vec_of_nodes, min_dist_node_id) {
  adjacent_nodes <- weighted_adj_list[[min_dist_node_id]]
  adjacent_and_left_to_process <- intersect(names(adjacent_nodes), vec_of_nodes)
  return(adjacent_nodes[adjacent_and_left_to_process])
}

find_min_dist_node <- function(vec_of_nodes, vec_of_distances) {
  distances_left_vec <- distances_to_process(vec_of_nodes, vec_of_distances)
  min_distance <- min(distances_left_vec)
  min_dist_node_id <- get_min_dist_id(distances_left_vec, min_distance)
  result <- list('node_id' = min_dist_node_id, 'current_distance_to' = min_distance)
  return(result)
}

distances_to_process <- function(vec_of_nodes, vec_of_distances) {
  distances_left <- intersect(names(vec_of_distances), vec_of_nodes)
  distances_left_vec <- vec_of_distances[distances_left]
  return(distances_left_vec)
}

get_min_dist_id <- function(vec_of_distances, min_distance) {
  min_distance_idx <- match(min_distance, vec_of_distances)
  min_distance_node_id <- names(vec_of_distances[min_distance_idx])
  return(min_distance_node_id)
}

nodes_left_to_process <- function(vec_of_nodes) {
  num_nodes_not_na <- sum(!is.na(vec_of_nodes))
  return(num_nodes_not_na > 0)
}

create_predecessors_vec <- function(osm_graph) {
  num_nodes <- get_num_nodes(osm_graph)
  predecessors_vec <- rep(NA_character_, num_nodes)
  names(predecessors_vec) <- get_node_names(osm_graph)
  return(predecessors_vec)
}

get_num_nodes <- function(osm_graph) {
  return(vcount(osm_graph))
}

get_node_names <- function(osm_graph) {
  vertices <- V(osm_graph)
  return(names(vertices))
}

create_distances_vec <- function(osm_graph, source_node_id) {
  num_nodes <- vcount(osm_graph)
  distances_vec <- rep(LARGE_NUMBER, num_nodes)
  names(distances_vec) <- get_node_names(osm_graph)
  distances_vec <- initialise_distances_vector(distances_vec, source_node_id)
  return(distances_vec)
}

initialise_distances_vector <- function(distances_vector, source_node_id) {
  distances_vector[[source_node_id]] <- 0
  return(distances_vector)
}

recover_shortest_path <- function(source_node_id, destination_node_id, vec_of_predecessors, vec_of_distances) {
  path <- character()
  curr_node <- destination_node_id
  while(!is.na(vec_of_predecessors[curr_node])) {
    path <- append(path, curr_node[[1]])
    curr_node <- vec_of_predecessors[curr_node]
  }
  path <- append(path, source_node_id)
  path <- rev(path)
  distance_to_destination <- vec_of_distances[[destination_node_id]]
  return(list('path' = path, 'distance' = distance_to_destination))
}

