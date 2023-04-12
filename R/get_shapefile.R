#' @title Return shapefiles that are used in MACEReports
#' @description Return layers as \code{sf} objects (shapefiles) or xyz dataframes (rasters).
#' @param shapefile_name The name of a shapefile to access; current list is available with \code{available_shapefiles()}
#' @param crs The coordinate reference system (CRS) into which shapefiles will be projected. By default, objects will be returned using the default CRS Albers Equal Area Alaska, (EPSG 3338). Any other valid projection can be specified. Note that this relies on the \link[sf:st_crs]{sf::st_crs} function- make sure you are using a valid CRS argument.
#'
#' @examples
#' # return Alaska land in default CRS
#' ak_land <- get_shapefile(shapefile_name = "alaska_land")
#'
#' # return Alaska land in a different CRS
#' ak_land <- get_shapefile(shapefile_name = "alaska_land", crs = "EPSG:4326")
#' @export
get_shapefile <- function(shapefile_name,
                          crs = "EPSG:3338") {
  # checks: is it in the list of available names?

  # identify the mapping locations and files
  map_dir <- system.file("extdata/EPSG3338", package = "MACEReports")

  current_files <- list.files(map_dir)

  # make sure the requested file exists
  if (!any(stringr::str_detect(current_files, shapefile_name))) {
    stop(paste0(shapefile_name, " is not an option. Current shapefiles are: ", paste(current_files, collapse = ", ")))
  }

  # check: make sure crs specified correctly
  if (is.numeric(crs)) {
    stop("CRS must be specied as a character, i.e. 'EPSG:4326' ")
  }

  if (!stringr::str_detect(crs, "EPSG")) {
    stop("CRS must be specied as a character, i.e. 'EPSG:4326' ")
  }

  # check: shapefile must be a character
  if (!is.character(shapefile_name)) {
    stop("Shapefile name must be specified as a character")
  }

  # if it does, identify the requested file
  shapefile_loc <- current_files[which(stringr::str_detect(current_files, shapefile_name))]

  # if it is a .tif (raster), open with terra
  if (stringr::str_detect(shapefile_loc, ".tif")) {
    shapefile_object <- terra::rast(paste0(map_dir, "/", shapefile_loc))

    # convert the projection if requested
    if (crs != "EPSG:3338") {
      shapefile_object <- terra::project(shapefile_object, crs, method = "bilinear")

      # up the resolution
      shapefile_object <- terra::disagg(shapefile_object, fact = c(5, 5), method = "near")
    }

    # return as an xyz object for plotting
    shapefile_object <- terra::as.data.frame(shapefile_object, xy = TRUE) %>%
      dplyr::rename("z" = "lyr.1")
  }

  # otherwise, open with sf package
  if (!stringr::str_detect(shapefile_loc, ".tif")) {
    shapefile_object <- sf::st_read(paste0(map_dir, "/", shapefile_loc), quiet = TRUE)

    # convert the projection if requested
    if (crs != "EPSG:3338") {
      shapefile_object <- sf::st_transform(shapefile_object, crs = crs)
    }
  }

  # return the shapefile
  return(shapefile_object)
}
