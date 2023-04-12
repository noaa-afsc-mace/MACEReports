#' @title Return Alaska area labels for plotting
#' @description Return a \code{sf} spatial dataframe of area labels within a given plot extent.
#' @param plot_limits_data A \code{sf} spatial dataframe; this is required and used to define the base map extent and projection.
#' @param area_labels_list A (strongly recommended) list of labels to gather. All the available options are available using \code{MACEReports::available_labels()}
#' @return A dataframe of class \code{sf}, with projection as specifed by the \code{plot_limits_data}
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' # get a simple dataset to work from: biomass_nmi2 from a single survey
#' shelikof_xyz <- shelikof_biomass %>%
#'   dplyr::filter(year == 2021) %>%
#'   dplyr::group_by(INTERVAL, START_LATITUDE, START_LONGITUDE) %>%
#'   dplyr::summarize(
#'     BIOMASS = sum(BIOMASS),
#'     BIOMASS_NM2 = sum(BIOMASS_NM2)
#'   )
#'
#' # build a sticks object
#' sticks <- build_sf_sticks(
#'   x = shelikof_xyz$START_LONGITUDE,
#'   y = shelikof_xyz$START_LATITUDE,
#'   z = shelikof_xyz$BIOMASS_NM2
#' )
#'
#' # get a list of all the place names in your document
#' goa_labels_to_get <- c(
#'   "Shumagin Islands", "Alaska Peninsula", "Shelikof Strait",
#'   "Kodiak Island", "Cape Kekurnoi", "Cape Nukshak", "Korovin Island",
#'   "Kalsin Bay", "Cape Igvak"
#' )
#'
#' # use the limits of the sticks to get your labels in the plot
#' area_labels <- get_area_labels(plot_limits_data = sticks, area_labels_list = goa_labels_to_get)
#' }
#' @export
get_area_labels <- function(plot_limits_data = NULL,
                            area_labels_list = NULL) {
  # checks: Make sure we have a sf dataframe WITH a defined CRS for the plot data; stop if not.
  if (!"sf" %in% class(plot_limits_data) | is.na(sf::st_crs(plot_limits_data)$input)) {
    stop("Your plot data must be an sf spatial dataframe with a coordinate reference system (CRS)!")
  }

  # if an sf dataframe with a valid CRS is present, get the crs
  crs <- sf::st_crs(plot_limits_data)$input

  # check if we've already got a collection of shapefiles/rasters for the requested CRS
  map_dir <- system.file("extdata", stringr::str_remove(crs, ":"), package = "MACEReports")

  # if the directory exists- we just need to open up the requested files
  if (dir.exists(map_dir)) {
    alaska_area_labels <- sf::st_read(paste0(map_dir, "/alaska_area_labels.gpkg"), quiet = TRUE)

    if (!is.null(area_labels_list)) {
      # if there's a list of area labels provided, limit to these
      alaska_area_labels <- alaska_area_labels[alaska_area_labels$area_name %in% c(area_labels_list), ]
    }
  }

  # if we don't have anything for the requested crs, build it
  if (!dir.exists(map_dir)) {
    alaska_area_labels <- sf::st_read(paste0(base_dir, "/alaska_area_labels.gpkg"), quiet = TRUE)

    if (!is.null(area_labels_list)) {
      # if there's a list of area labels provided, limit to these
      alaska_area_labels <- alaska_area_labels[alaska_area_labels$area_name %in% c(area_labels_list), ]
    }
  }

  # clip the exent of background layers for plotting

  # limit the extent of the plot to be slightly greater than the plot area
  region_zoom_box <- sf::st_as_sfc(sf::st_bbox(plot_limits_data))

  # create a small buffer, around 10% of the total x-axis extent
  p_min <- sf::st_point(c(min(sf::st_coordinates(region_zoom_box)[, 1]), min(sf::st_coordinates(region_zoom_box)[, 2])))
  p_max <- sf::st_point(c(max(sf::st_coordinates(region_zoom_box)[, 1]), max(sf::st_coordinates(region_zoom_box)[, 2])))

  # compute the maximum distance across plot; add a buffer to the plot as 25% of this distance to get labels at edges
  dist_buffer <- sf::st_distance(p_min, p_max)[[1]] * 0.1

  # if you are working in a geographic coordinate system, add this buffer directly to your coordinates
  # (this is a workaround, as geographic buffers are problematic- not needed if you are in a projected system)
  if (sf::st_is_longlat(region_zoom_box)) {
    sf::sf_use_s2(FALSE)
    region_zoom_box <- sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = "MITRE", mitreLimit = 2)
    sf::sf_use_s2(TRUE)
  }

  # #in projected coordinate systems, simply apply the buffer
  if (!sf::st_is_longlat(region_zoom_box)) {
    region_zoom_box <- sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = "MITRE", mitreLimit = 2)
  }

  # limit the points to those in the region zoom box
  alaska_area_labels <- alaska_area_labels[region_zoom_box, ]

  # return the area labels
  return(alaska_area_labels)
}
