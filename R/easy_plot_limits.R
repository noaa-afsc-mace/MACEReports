#' @title Set your ggplot map limits a bit more easily
#' @description Returns a \code{ggplot2} object that will limit the extent of your ggplot map to cover only the data you care about. This will be slightly larger than the extent of \code{plot_limits_data}; users can still use \code{ggplot2::coord_sf} to fine-tune the plot extent.
#' @param plot_limits_data A \code{sf} spatial dataframe; this is required and used to map extent. Use the same dataframe as you used for \code{get_basemap_layers}!
#' @param plot_expansion This controls the amount of buffer around the basemap (default is 5\% (0.05) around the extent of the data; set from 0-1). This should match the value used for \code{get_basemap_layers}.
#' @return a \code{ggplot2} object that will limit the extent of your ggplot map.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(sf)
#' library(MACEReports)
#'
#' dat <- data.frame(
#'   "x" = c(-152.2, -150.3, -159.4),
#'   "y" = c(55.2, 55.8, 55.6),
#'   "z" = c(7500, 40000, 28000),
#'   "species" = c("a", "a", "b")
#' )
#'
#' # create an sf dataframe
#' dat <- sf::st_as_sf(dat, coords = c("x", "y"), crs = 4326)
#'
#' # convert CRS to a reasonable projection
#' dat <- sf::st_transform(dat, crs = "EPSG:3338")
#'
#' # return the basemap as a ggplot layer
#' mace_basemap <- get_basemap_layers(plot_limits_data = dat)
#'
#' # now go get some other bathymetry
#` bathy_data <- MACEReports::get_shapefile(shapefile_name = "alaska_bathy_contours") %>%
#`  filter(METERS %in% c(50,100,200,1000))
#`
#` # and plot it
#` basemap +
#`   geom_sf(data = bathy_data, aes(linetype = factor(METERS))) +
#`   easy_plot_limits(plot_limits_data = sticks)
#'}
#'@export
easy_plot_limits <- function(plot_limits_data,
                             plot_expansion =  0.05){

  # checks: Make sure we have a sf dataframe WITH a defined CRS for the plot data; stop if not.
  if (!"sf" %in% class(plot_limits_data) | is.na(sf::st_crs(plot_limits_data)$input)) {
    stop("Your plot data must be an sf spatial dataframe with a coordinate reference system (CRS)!")
  }

  # limit the extent of the plot to be slightly greater than the plot area
  region_zoom_box <- sf::st_as_sfc(sf::st_bbox(plot_limits_data))

  # create a small buffer, around 10% of the total x-axis extent
  p_min <- sf::st_point(c(min(sf::st_coordinates(region_zoom_box)[, 1]), min(sf::st_coordinates(region_zoom_box)[, 2])))
  p_max <- sf::st_point(c(max(sf::st_coordinates(region_zoom_box)[, 1]), max(sf::st_coordinates(region_zoom_box)[, 2])))

  # compute the maximum distance across plot; add a buffer to the plot as n% of this distance
  dist_buffer <- sf::st_distance(p_min, p_max)[[1]] * plot_expansion

  # if you are working in a geographic coordinate system, add this buffer directly to your coordinates
  # (this is a workaround, as geographic buffers are problematic- not needed if you are in a projected system)
  if (sf::st_is_longlat(region_zoom_box)) {
    sf::sf_use_s2(FALSE)
    region_zoom_box <- sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = "MITRE", mitreLimit = 2)
    sf::sf_use_s2(TRUE)
  }

  # in projected coordinate systems, simply apply the buffer
  if (!sf::st_is_longlat(region_zoom_box)) {
    region_zoom_box <- sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = "MITRE", mitreLimit = 2)
  }

  # return the plot limits based on the plotted data
  easy_lims <- ggplot2::coord_sf(label_axes = "--EN",
  xlim = c(min(sf::st_coordinates(region_zoom_box)[,1]), max(sf::st_coordinates(region_zoom_box)[,1])),
  ylim = c(min(sf::st_coordinates(region_zoom_box)[,2]), max(sf::st_coordinates(region_zoom_box)[,2])),
  expand = FALSE)

}
