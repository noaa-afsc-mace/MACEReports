#' @title A list of the available shapefiles from MACEReports
#' @return prints the available shapefiles from MACEReports
#' @export
available_shapefiles <- function(){

  #open up the labels shapefile
  map_dir = system.file("extdata/EPSG3338", package = "MACEReports")

  #print the available shapefiles
  all_files = list.files(map_dir)

  for (i in 1:length(all_files)){

    print(stringr::str_split(all_files, '_EPSG')[[i]][1])

  }

}
