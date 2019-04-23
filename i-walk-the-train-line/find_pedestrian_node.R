
find_pedestrian_node <- function(osm_data, node) {
  node <- find_nearest_node(osm_data, node(node), way(tags(k == "highway" & v == "pedestrian")))
  nearest_highway_node <- subset(osm_data, node_ids = node)
  return(nearest_highway_node)
}
