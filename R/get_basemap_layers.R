#' @title Create a MACE-themed basemap
#' @description Returns a base map. This map is returned as a ggplot2 object that more complex maps can be built on top of.
#' It provides land, bathymetry, and, optionally, a variety of common layers including the NMFS management areas, 3 NMI buffer #' regions,and Steller Sea Lion exclusions. These basemaps are intended for the Bering Sea and Gulf of Alaska.
#' This layer will be slightly larger than the extent of \code{plot_limits_data}; users can use \code{plot_expansion} parameter to fine-tune the extent.
#' @param plot_limits_data A \code{sf} spatial dataframe; this is required and used to define the base map extent and projection.
#' @param bathy By default, a bathymetric baselayer based on the GEBCO (https://www.gebco.net/) gridded bathymetric dataset
#' is included in the basemap; If \code{FALSE}, bathymetric baselayer will not be included
#' @param bathy_max_plot_depth The maximum depth you want to emphasize in the bathymetric color scale (default to 1000 m); this
#' helps to focus the contrast at shallower depths and not emphasize especially deep areas off of the shelf break.
#' @param contours Provide contour lines at requested depths. Specify depths as positive numeric values,
#' i.e. \code{c(200,100,50)}
#' @param management_regions If \code{TRUE}, will add NMFS management regions to basemap
#' @param SSL_critical_habitat If \code{TRUE}, will add Steller Sea Lion critical habitat buffers to basemap
#' @param alaska_3nmi_buffer If \code{TRUE}, will add the ADFG 3 nmi management buffer to basemap
#' @param land_fill_color If you'd like a different fill color on landmasses, specify as required by \code{ggplot2}.
#' @param land_outline_color If you'd like a different outline color on landmasses, specify as required by \code{ggplot2}.
#' @param plot_expansion This controls the amount of buffer around the basemap (default is 5\% (0.05) around the extent of the data; set from 0-1).
#'
#' @return A list of class \code{ggplot} containing information required for plotting a basemap.
#'
#' @author Mike Levine
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
#' # note that this basemap will replace the usual call to ggplot() as the start of your plot:
#' mace_basemap +
#'   geom_sf(data = dat, aes(color = species))
#'
#' # You can add other commonly used MACE layers to the basemap
#' mace_basemap <- get_basemap_layers(plot_limits_data = dat,
#' management_regions = TRUE)
#' mace_basemap
#'
#' mace_basemap <- get_basemap_layers(plot_limits_data = dat,
#' alaska_3nmi_buffer = TRUE)
#' mace_basemap
#'
#' mace_basemap <- get_basemap_layers(plot_limits_data = dat,
#' SSL_critical_habitat = TRUE)
#' mace_basemap
#'
#' mace_basemap <- get_basemap_layers(plot_limits_data = dat,
#' bathy = FALSE,
#' contours = c(200, 100, 50))
#' mace_basemap
#' }
#' @export
get_basemap_layers <- function(plot_limits_data,
                               bathy = TRUE,
                               bathy_max_plot_depth = 1000,
                               contours = NULL,
                               management_regions = NULL,
                               SSL_critical_habitat = NULL,
                               alaska_3nmi_buffer = NULL,
                               land_fill_color = "#616161",
                               land_outline_color = "black",
                               plot_expansion = 0.05) {
  # checks: Make sure we have a sf dataframe WITH a defined CRS for the plot data; stop if not.
  if (!"sf" %in% class(plot_limits_data) | is.na(sf::st_crs(plot_limits_data)$input)) {
    stop("Your plot data must be an sf spatial dataframe with a coordinate reference system (CRS)!")
  }

  # if an sf dataframe with a valid CRS is present, get the crs
  crs <- sf::st_crs(plot_limits_data)$input

  # this is a temporary fix to deal with SF changes- it points the most common case (EPSG:3338) to its pre-existing folder
  if (crs == "NAD83 / Alaska Albers"){
    crs <- "EPSG:3338"
  }

  # check if we've already got a collection of shapefiles/rasters for the requested CRS
  # map_dir = paste0('inst/extdata/', stringr::str_remove(crs, ':'))
  map_dir <- system.file("extdata", stringr::str_remove(crs, ":"), package = "MACEReports")

  # if the directory exists- we just need to open up the requested files
  if (dir.exists(map_dir)) {
    # open up all the layers we need
    ak_land <- sf::st_read(paste0(map_dir, "/alaska_land_", stringr::str_remove(crs, ":"), ".gpkg"), quiet = TRUE)
    russia_land <- sf::st_read(paste0(map_dir, "/russia_land_", stringr::str_remove(crs, ":"), ".gpkg"), quiet = TRUE)
    canada_land <- sf::st_read(paste0(map_dir, "/canada_land_", stringr::str_remove(crs, ":"), ".gpkg"), quiet = TRUE)

    if (!is.null(management_regions)) {
      management_regions_layer <- sf::st_read(paste0(
        map_dir, "/alaska_NMFS_management_regions_",
        stringr::str_remove(crs, ":"), ".gpkg"
      ), quiet = TRUE)
    }

    if (!is.null(SSL_critical_habitat)) {
      SSL_critical_habitat_layer <- sf::st_read(paste0(
        map_dir, "/SSL_critical_habitat_",
        stringr::str_remove(crs, ":"), ".gpkg"
      ), quiet = TRUE)
    }

    if (!is.null(alaska_3nmi_buffer)) {
      alaska_3nmi_buffer_layer <- sf::st_read(paste0(
        map_dir, "/alaska_3nmi_buffer_",
        stringr::str_remove(crs, ":"), ".gpkg"
      ), quiet = TRUE)
    }

    if (bathy == TRUE) {
      bathy_raster <- terra::rast(paste0(map_dir, "/alaska_bathy_raster_", stringr::str_remove(crs, ":"), ".tif"))
    }

    # if user requests contours instead of full bathy, produce these
    if (!is.null(contours)) {
       # check: make sure it is a numeric object
       if (!is.numeric(contours)){
         stop('Enter the contours you want, as in c(200,300). Options are: 50, 100, 200, 400, 500, 600, 800, 1000.')
       }

       #if the contours are provided as negative values, set as positive
       contours = ifelse(contours > 0, contours, -contours)

       #open the contours file
       bathy_contours = sf::st_read(paste0(map_dir, '/alaska_bathy_contours_',
                                           stringr::str_remove(crs, ':'), '.gpkg'), quiet = TRUE)

       # limit to the requested contour values
       bathy_contours = bathy_contours[bathy_contours$METERS %in% contours,]

    }
  }

  # if we don't have anything for the requested crs, build it
  if (!dir.exists(map_dir)) {
    message(paste0("Creating new basemap features for ", crs, "."))

    # open the shapefiles (from 3338- since these are included to start with)
    base_dir <- system.file("extdata/EPSG3338/", package = "MACEReports")
    ak_land <- sf::st_read(paste0(base_dir, "/alaska_land_EPSG3338.gpkg"), quiet = TRUE)
    russia_land <- sf::st_read(paste0(base_dir, "/russia_land_EPSG3338.gpkg"), quiet = TRUE)
    canada_land <- sf::st_read(paste0(base_dir, "/canada_land_EPSG3338.gpkg"), quiet = TRUE)
    management_regions_layer <- sf::st_read(paste0(base_dir, "/alaska_NMFS_management_regions_EPSG3338.gpkg"), quiet = TRUE)
    SSL_critical_habitat_layer <- sf::st_read(paste0(base_dir, "/SSL_critical_habitat_EPSG3338.gpkg"), quiet = TRUE)
    alaska_3nmi_buffer_layer <- sf::st_read(paste0(base_dir, "/alaska_3nmi_buffer_EPSG3338.gpkg"), quiet = TRUE)
    bathy_contours <- sf::st_read(paste0(map_dir, '/alaska_bathy_contours_', stringr::str_remove(crs, ':'), '.gpkg'), quiet = TRUE)

    # convert to the requested projection
    ak_land <- sf::st_transform(ak_land, crs = crs)
    russia_land <- sf::st_transform(russia_land, crs = crs)
    canada_land <- sf::st_transform(canada_land, crs = crs)
    management_regions_layer <- sf::st_transform(management_regions_layer, crs = crs)
    SSL_critical_habitat_layer <- sf::st_transform(SSL_critical_habitat_layer, crs = crs)
    alaska_3nmi_buffer_layer <- sf::st_transform(alaska_3nmi_buffer_layer, crs = crs)
    bathy_contours <- sf::st_transform(bathy_contours, crs = crs)

    if (bathy == TRUE) {
      # again, start with the 3338 layer
      bathy_raster <- terra::rast(paste0(base_dir, "/alaska_bathy_raster_EPSG3338.tif"))

      # convert it
      bathy_raster <- terra::project(bathy_raster, crs, method = "bilinear")

      # up the resolution
      bathy_raster <- terra::disagg(bathy_raster, fact = c(5, 5), method = "near")
    }

    if (!is.null(contours)) {
       #check: make sure it is a numeric object
       if (!is.numeric(contours)){
         stop('Enter the contours you want, as in c(200,300). Options are: 50, 100, 200, 300, 500, 700, 1000.')
       }

       # if the contours are provided as negative values, set as positive
       contours = ifelse(contours > 0, contours, -contours)

       # open up the contours
       bathy_contours = sf::st_read(paste0(base_dir, '/alaska_bathy_contours_EPSG3338.gpkg'), quiet = TRUE)

       # limit to the requested contour values
       bathy_contours = bathy_contours[bathy_contours$METERS %in% contours,]

       # convert to the requested projection
       bathy_contours = sf::st_transform(bathy_contours, crs = crs)

    }
  }

  # clip the exent of background layers for plotting

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

  # #in projected coordinate systems, simply apply the buffer
  if (!sf::st_is_longlat(region_zoom_box)) {
    region_zoom_box <- sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = "MITRE", mitreLimit = 2)
  }

  # crop the base layers
  ak_land <- sf::st_intersection(sf::st_geometry(ak_land), sf::st_geometry(region_zoom_box))
  russia_land <- sf::st_intersection(sf::st_geometry(russia_land), sf::st_geometry(region_zoom_box))
  canada_land <- sf::st_intersection(sf::st_geometry(canada_land), sf::st_geometry(region_zoom_box))

  # get the automatically-generated graticule for the biggest shapefile (ak_land)- we will use this to force more longitude ticks on the basemap if there are an insufficient (<3) number of ticks.
  grats <- sf::st_graticule(ak_land)

  if (!is.null(management_regions)) {
    management_regions_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(management_regions_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(SSL_critical_habitat)) {
    SSL_critical_habitat_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(SSL_critical_habitat_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(alaska_3nmi_buffer)) {
    alaska_3nmi_buffer_layer <- sf::st_intersection(sf::st_geometry(alaska_3nmi_buffer_layer), sf::st_geometry(region_zoom_box))
  }


  # if we are using bathy raster, limit it to be sized for the plot too (much faster);
  # and return it as a dataframe to plot with ggplot
  if (bathy == TRUE) {
    # set a box that can be used to clip raster to plot extent (much faster plotting!)
    clip <- terra::ext(
      sf::st_bbox(region_zoom_box)[[1]], sf::st_bbox(region_zoom_box)[[3]],
      sf::st_bbox(region_zoom_box)[[2]], sf::st_bbox(region_zoom_box)[[4]]
    )

    # crop to these dimensions
    bathy_raster_df <- terra::crop(bathy_raster, clip, snap = "near")

    bathy_raster_df <- terra::as.data.frame(bathy_raster_df, xy = TRUE) %>%
      dplyr::rename("z" = "lyr.1")

    # define a color scale- you can tweak this to give the right amount of weight to the deep vs shallow areas (right now),
    # theres more weight given to 250 m and up

    # to maximize visibility only display negative (i.e. depth) values from -1000 m - 0 m
    bathy_raster_df$z <- ifelse(bathy_raster_df$z < -bathy_max_plot_depth, -bathy_max_plot_depth, bathy_raster_df$z)
    bathy_raster_df$z <- ifelse(bathy_raster_df$z > 0, 1, bathy_raster_df$z)

    bathy_colors <- ggplot2::scale_fill_gradientn(
      values = scales::rescale(c(
        min(bathy_raster_df$z, na.rm = TRUE),
        -250, -50, .99,
        max(bathy_raster_df$z, na.rm = TRUE)
      )),
      colors = c("#737373", "#969696", "#d9d9d9", "#d9d9d9"),
      # if user wants a legend, present units as positive (depth)
      # instead of negative (altitude)
      labels = function(x) {
        abs(x)
      }
    )
  }

  if (!is.null(contours)) {
    # crop the contour layer
    bathy_contours <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(bathy_contours)),
      sf::st_geometry(region_zoom_box)
    )
  }

  # define a color for the added layers (management regions, etc): white if bathy, black if contours
  layer_col <- ifelse(bathy == TRUE, "white", "black")

  # define the basemap ggplot object
  basemap_layers <- ggplot2::ggplot() +
    {
      if (bathy == TRUE) ggplot2::geom_raster(data = bathy_raster_df, ggplot2::aes(x = x, y = y, fill = z))
    } +
    {
      if (bathy == TRUE) bathy_colors
    } +
    {
      if (!is.null(contours) & bathy != TRUE) ggplot2::geom_sf(data = bathy_contours, color = "gray60")
    } +
    {
      if (!is.null(SSL_critical_habitat)) ggplot2::geom_sf(data = SSL_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(alaska_3nmi_buffer)) ggplot2::geom_sf(data = alaska_3nmi_buffer_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(management_regions)) ggplot2::geom_sf(data = management_regions_layer, color = layer_col, fill = "transparent")
    } +
    ggplot2::geom_sf(data = ak_land, fill = land_fill_color, color = land_outline_color) +
    ggplot2::geom_sf(data = russia_land, fill = land_fill_color, color = land_outline_color) +
    ggplot2::geom_sf(data = canada_land, fill = land_fill_color, color = land_outline_color) +
    {
      if (bathy == TRUE) ggplot2::guides(fill = "none")
    } +
    # adjust x-axis labels- either add a few (if there are <3), or accept defaults (if >3)
    {
      if (length(grats$type[grats$type == "E"]) < 3) ggplot2::scale_x_continuous(breaks = seq(-180, 180, 5), expand = c(0, 0))
    } +
    {
      if (length(grats$type[grats$type == "E"]) >= 3) ggplot2::scale_x_continuous(expand = c(0, 0))
    } +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    # set the basemap to standard MACE theme
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12),
      legend.box.background = ggplot2::element_rect(fill = scales::alpha("white", 0.55), color = "transparent"),
      legend.key = ggplot2::element_rect(fill = "transparent"),
      legend.text = ggplot2::element_text(color = "black", size = 12),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

  return(basemap_layers)
}
