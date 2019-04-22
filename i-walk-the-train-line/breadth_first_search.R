
breadth_first_search <- function(adjacency_list, source_node, destination_node) {
  require(rstackdeque)
  
  source_node <- get_node_id(source_node)
  destination_node <- get_node_id(destination_node)
  
  # some initial checks  
  source_node_in_list(source_node, adjacency_list)
  destination_node_in_list(source_node, adjacency_list)
  
  # initialise
  found <- FALSE
  vertices <-names(adjacency_list)
  visited_nodes <- initialise_visited_nodes(adjacency_list, source_node)
    
  # create our empty queue and enqueue source node
  q <- initialise_queue(source_node)
  
  while(nodes_to_process(found, q)) {
    dequeued_node <- peek_front(q)
    q <- without_front(q)
    
    if (found_destination_node(dequeued_node, destination_node)) {
      found <- TRUE
    } else {
      adjacent_nodes <- get_adjacent_nodes(adjacency_list, dequeued_node)
      for (child_node in adjacent_nodes) {
        if (not_visited_node_yet(visited_nodes, child_node)) {
          q <- enqueue_child(q, child_node)
          visited_nodes <- mark_child_as_visited(visited_nodes, child_node, dequeued_node)
        }
      }
    }
  }
  
  # if we still have not found our path, it does not exist
  if (!found) {
    print('path not found')
    return()
  }
  
  # otherwise, recover the path from destination to source
  path <- character()
  current_node <- destination_node
  path <- append(path, current_node)
  
  while (not_reached_source_node(visited_nodes, current_node, source_node)) {
    current_node <- get_next_node(visited_nodes, current_node)
    path <- append(path, current_node)
  }
  path <- append(path, source_node)
  
  # and then reverse it!
  path <- rev(path)
  
  return(path)
}


get_node_id <- function(node) {
  node_id <- as.character(node$nodes$attrs$id)
  return(node_id)
}


source_node_in_list <- function(source_node, adjacency_list) {
  if (!source_node %in% names(adjacency_list)) {
    stop('source node not in this graph...')
  }
}

destination_node_in_list <- function(destination_node, adjacency_list) {
  if (!destination_node %in% names(adjacency_list)) {
    print('destination node not in this graph...')
    return()
  }
}

initialise_visited_nodes <- function(adjacency_list, source_node) {
  visited_nodes <- rep(NA_character_, length(adjacency_list))
  names(visited_nodes) <- names(adjacency_list)
  # initialise source node predssor as itself
  visited_nodes[source_node] <- source_node
  return(visited_nodes)
}

initialise_queue <- function(source_node) {
  q <- rpqueue()
  q <- insert_back(q, source_node)
  return(q)
}

nodes_to_process <- function(found, q) {
  return(!found | !empty(q))  
}

found_destination_node <- function(dequeued_node, destination_node) {
  return(dequeued_node == destination_node)
}

get_next_node <- function(visited_nodes, current_node) {
  current_node <- visited_nodes[[current_node]]
  return(current_node)
}

get_adjacent_nodes <- function(adjacency_list, dequeued_node) {
  return(names(adjacency_list[[dequeued_node]]))
}

not_visited_node_yet <- function(visited_nodes, child_node) {
  return(is.na(visited_nodes[child_node]))
}

enqueue_child <- function(q, child_node) {
  return(insert_back(q, child_node))
}

mark_child_as_visited <- function(visited_nodes, child_node, dequeued_node) {
  visited_nodes[child_node] <- dequeued_node
  return(visited_nodes)
}

not_reached_source_node <- function(visited_nodes, current_node, source_node) {
  return(visited_nodes[[current_node]] != source_node)
}
