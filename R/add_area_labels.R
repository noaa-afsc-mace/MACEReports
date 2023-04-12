#' @title Add area labels to the big collection of labels
#'
#' @description Adds a label to the collection given latitude, longitude, and a name.
#' @keywords internal
#' @param lat_dd Latitude (decimal degree)
#' @param lon_dd Longitude (decimal degree)
#' @param area_name Area name (character)
#'
#' @return None- adds a single row to the \code{'alaska_area_labels.gpkg'} file.
#'
#' @author Mike Levine
#'
#' @export
add_area_labels <- function(lat_dd, lon_dd, area_name) {
  # data checks:

  # lat_dd,lon_dd must be numeric
  if (!is.numeric(c(lat_dd, lon_dd))) stop("lat_dd and lon_dd must all be numeric")

  # area_name must be a character
  if (!is.character(c(area_name))) stop("area_name must be a character")

  # open up the labels shapefile (note that this will be in EPSG:3338/Alaska equal area Albers)
  map_dir <- system.file("extdata/EPSG3338", package = "MACEReports")
  alaska_area_labels <- sf::st_read(paste0(map_dir, "/alaska_area_labels.gpkg"), quiet = TRUE)

  # prep the new label for adding- make an sf object (CRS 4326 as it is lat_dd/lon_dd)
  new_label <- data.frame(lat_dd, lon_dd, area_name)
  new_label <- sf::st_as_sf(new_label, coords = c("lon_dd", "lat_dd"), crs = 4326)

  # convert it to the crs of the area_labels shapefile
  new_label <- sf::st_transform(new_label, crs = sf::st_crs(alaska_area_labels))

  # to be safe- force the column names in the new labels to match for merging
  sf::st_geometry(new_label) <- "geom"

  # add it to the collection of labels
  alaska_area_labels <- rbind(alaska_area_labels, new_label)

  # save the updated area labels
  sf::st_write(alaska_area_labels, dsn = "inst/extdata/EPSG3338/alaska_area_labels.gpkg", delete_dsn = TRUE)
}
