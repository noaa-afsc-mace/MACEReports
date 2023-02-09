#' @title A list of the available Alaska area labels (900 +!)
#' @description The available area labels in alphabetical order
#' @return A dataframe with 1 column of label names
#' @export
available_labels = function(){

  #open up the labels shapefile
  map_dir = system.file("extdata/EPSG3338", package = "MACEReports")
  alaska_area_labels = sf::st_read(paste0(map_dir, '/alaska_area_labels.gpkg'), quiet = TRUE)

  #return them all in alphabetical order
  labels = alaska_area_labels%>%
    sf::st_drop_geometry()%>%
    dplyr::arrange(OBJNAM)

  return(labels)
}
