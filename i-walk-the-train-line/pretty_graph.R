
pretty_graph <- function(osm_data, start = NULL, end = NULL, path_nodes = NULL,
                         node_alpha = 0.1, edge_alpha = 0.5) {
  
  require(dplyr)
  require(ggplot2)
  # get node coordinates
  nodes <- osm_data$nodes$attrs
  nodes$timestamp <- NULL
  node_lat <- nodes$lat
  node_lon <- nodes$lon
  node_coords <- data.frame(lat = node_lat, lon = node_lon)
  
  # highways <- find(osm_data, way(tags(k == 'highway')))
  
  # if path_nodes provided, find the node ids
  if (!is.null(path_nodes)) {
    path_nodes <- find_up(osm_data, node(path_nodes))
    path_nodes <- subset(osm_data, ids = path_nodes)
    path_nodes <- path_nodes$nodes$attrs %>%
      select(lat, lon)
  }  
  
  # get ways coordinates
  ways <- osm_data$ways$refs
  ways <- left_join(ways, nodes, by = c('ref' = 'id'))
  
  ways <- ways %>%
    select(ref, id, lat, lon) %>%
    group_by(id) %>%
    mutate(lon_end = lead(lon), lat_end = lead(lat)) %>%
    filter(!is.na(lon_end) & !is.na(lat_end))
  
  pretty_plot <- ggplot(node_coords, aes(x = lon, y = lat)) +
    geom_segment(data = ways, aes(x = lon, y = lat, xend = lon_end, yend = lat_end),
                 linetype = 'dashed', alpha = edge_alpha) +
    labs(x = 'Longitude', y = 'Latitude')
  
  if(!is.null(start)) {
    start <- start$nodes$attrs %>%
      select(lat, lon)
    pretty_plot <- pretty_plot + 
      geom_point(data = start, aes(x = lon, y = lat), colour = 'green', size = 5)
  }
  
  if(!is.null(end)) {
    end <- end$nodes$attrs %>%
      select(lat, lon)
    pretty_plot <- pretty_plot + 
      geom_point(data = end, aes(x = lon, y = lat), colour = 'red', size = 5)
  }
  
  if (!is.null(path_nodes)) {
    pretty_plot <- pretty_plot + 
      geom_point(data = path_nodes, aes(x = lon, y = lat), colour = 'blue', size = 2,
                 alpha = node_alpha)
  }
  
  # plot the start and end node last so that they appear on top of all other coloured nodes
  print(pretty_plot)
}
