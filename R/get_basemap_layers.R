#' @title Create a MACE-themed basemap
#' @description Returns a base map. This map is returned as a ggplot2 object that more complex maps can be built on top of.
#' It provides land, bathymetry, and, optionally, a variety of common layers including the NMFS management areas, 3 NMI buffer #' regions,and Critical Habitat and No Transit zones for ESA-listed species (these layers were gathered from https://www.fisheries.noaa.gov/resource/map/national-esa-critical-habitat-mapper by Abigail McCarthy in 2024) . These basemaps are intended for the Bering Sea and Gulf of Alaska.
#' This layer will be slightly larger than the extent of \code{plot_limits_data}; users can use \code{plot_expansion} parameter to fine-tune the extent.
#' @param plot_limits_data A \code{sf} spatial dataframe; this is required and used to define the base map extent and projection.
#' @param bathy By default, a bathymetric baselayer based on the GEBCO (https://www.gebco.net/) gridded bathymetric dataset
#' is included in the basemap; If \code{FALSE}, bathymetric baselayer will not be included
#' @param bathy_max_plot_depth The maximum depth you want to emphasize in the bathymetric color scale (default to 1000 m); this
#' helps to focus the contrast at shallower depths and not emphasize especially deep areas off of the shelf break.
#' @param contours Provide contour lines at requested depths. Specify depths as positive numeric values,
#' i.e. \code{c(200,100,50)}
#' @param contour_linescale Make each contour line a different type (solid, dashed, etc). Default is true. Note that the plot does not include the values in a legend, because MACE plots are already busy enough and we typically denote the values in captions. Default is TRUE.
#' @param management_regions If \code{TRUE}, will add NMFS management regions to basemap
#' @param alaska_3nmi_buffer If \code{TRUE}, will add the ADFG 3 nmi management buffer to basemap
#' @param land_fill_color If you'd like a different fill color on landmasses, specify as required by \code{ggplot2}.
#' @param land_outline_color If you'd like a different outline color on landmasses, specify as required by \code{ggplot2}.
#' @param plot_expansion This controls the amount of buffer around the basemap (default is 5\% (0.05) around the extent of the data; set from 0-1).
#' @param SSL_critical_habitat If \code{TRUE}, will add Steller Sea Lion critical habitat buffers to basemap
#' @param SSL_no_transit If \code{TRUE}, will add Steller Sea Lion no transit zones to basemap
#' @param humpback_critical_habitat If \code{TRUE}, will add the Humpack whale DPS critical habitat to basemap
#' @param NPRW_critical_habitat If \code{TRUE}, will add the North Pacific Right Whale critical habitat to basemap
#' @param sea_otter_critical_habitat If \code{TRUE}, will add Sea Otter critical habitat to basemap
#' @param CI_beluga_critical_habitat If \code{TRUE}, will add Cook Inlet Beluga critical habitat to basemap
#' @param walrus_protection_area If \code{TRUE}, will add Pacific Walrus protection area to basemap
#' @param walrus_no_transit If \code{TRUE}, will add Round Island Pacific Walrus no transit area to basemap
#' @param NWR_Afognak_Semidi_boundaries If \code{TRUE}, will add the National Wildlife Refuge Afognak and Semidi boundaries to no transit area to basemap
#' @param ringed_seal_critical_habitat If \code{TRUE}, will add Ringed Seal critical habitat to basemap
#' @param bearded_seal_critical_habitat If \code{TRUE}, will add Bearded Seal critical habitat to basemap
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
                               contour_linescale = TRUE,
                               management_regions = NULL,
                               alaska_3nmi_buffer = NULL,
                               land_fill_color = "#616161",
                               land_outline_color = "black",
                               plot_expansion = 0.05,
                               SSL_critical_habitat = NULL,
                               SSL_no_transit = NULL,
                               humpback_critical_habitat = NULL,
                               NPRW_critical_habitat = NULL,
                               sea_otter_critical_habitat = NULL,
                               CI_beluga_critical_habitat = NULL,
                               walrus_protection_area = NULL,
                               walrus_no_transit = NULL,
                               NWR_Afognak_Semidi_boundaries = NULL,
                               ringed_seal_critical_habitat = NULL,
                               bearded_seal_critical_habitat = NULL) {
  # checks: Make sure we have a sf dataframe WITH a defined CRS for the plot data; stop if not.
  if (!"sf" %in% class(plot_limits_data) | is.na(sf::st_crs(plot_limits_data)$input)) {
    stop("Your plot data must be an sf spatial dataframe with a coordinate reference system (CRS)!")
  }

  # if an sf dataframe with a valid CRS is present, get the crs
  input_crs <- sf::st_crs(plot_limits_data)$input

  # this is a temporary fix to deal with SF changes- it points the most common case (EPSG:3338) to its pre-existing folder
  if (input_crs == "NAD83 / Alaska Albers"){
    input_crs <- "EPSG:3338"
  }

  # strongly suggest that the user picks a projected coordinate system if they haven't
  if (isTRUE(sf::st_is_longlat(input_crs))){
    warning('You are working with Lat/Lon coordinates (using ', input_crs, '). This is likely to lead to some ugly maps! You should really consider using a projected coordinate system! Try EPSG:3338 for Alaska.')
  }

  # open up the EPSG:3338 collection of shapefiles (we'll reproject if needed below)
  map_dir <- system.file("extdata/EPSG3338", package = "MACEReports")

  # open up all the layers we need
  ak_land <- sf::st_read(paste0(map_dir, "/alaska_land_EPSG3338.gpkg"), quiet = TRUE)
  russia_land <- sf::st_read(paste0(map_dir, "/russia_land_EPSG3338.gpkg"), quiet = TRUE)
  canada_land <- sf::st_read(paste0(map_dir, "/canada_land_EPSG3338.gpkg"), quiet = TRUE)

  if (!is.null(management_regions)) {
    management_regions_layer <- sf::st_read(paste0(
      map_dir, "/alaska_NMFS_management_regions_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(SSL_critical_habitat)) {
    SSL_critical_habitat_layer <- sf::st_read(paste0(
      map_dir, "/SSL_critical_habitat_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(SSL_no_transit)) {
    SSL_no_transit_layer <- sf::st_read(paste0(
      map_dir, "/SSL_no_transit_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(humpback_critical_habitat)) {
    humpback_critical_habitat_layer <- sf::st_read(paste0(
      map_dir, "/humpback_whale_dps_critical_habitat_2021_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(NPRW_critical_habitat)) {
    NPRW_critical_habitat_layer <- sf::st_read(paste0(
      map_dir, "/NPRW_critical_habitat_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(sea_otter_critical_habitat)) {
    sea_otter_critical_habitat_layer <- sf::st_read(paste0(
      map_dir, "/sea_otter_critical_habitat_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(CI_beluga_critical_habitat)) {
    CI_beluga_critical_habitat_layer <- sf::st_read(paste0(
      map_dir, "/ci_beluga_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(walrus_no_transit)) {
    walrus_no_transit_layer <- sf::st_read(paste0(
      map_dir, "/walrus_no_transit_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(walrus_protection_area)) {
    walrus_protection_area_layer <- sf::st_read(paste0(
      map_dir, "/walrus_protection_area_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(NWR_Afognak_Semidi_boundaries)) {
    NWR_Afognak_Semidi_boundaries_layer <- sf::st_read(paste0(
      map_dir, "/NWR_Afognak_Semidi_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(ringed_seal_critical_habitat)) {
    ringed_seal_critical_habitat_layer <- sf::st_read(paste0(
      map_dir, "/ringed_seal_critical_habitat_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(bearded_seal_critical_habitat)) {
    bearded_seal_critical_habitat_layer <- sf::st_read(paste0(
      map_dir, "/bearded_seal_critical_habitat_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (!is.null(alaska_3nmi_buffer)) {
    alaska_3nmi_buffer_layer <- sf::st_read(paste0(
      map_dir, "/alaska_3nmi_buffer_EPSG3338.gpkg"
    ), quiet = TRUE)
  }

  if (bathy == TRUE) {
    bathy_raster <- terra::rast(paste0(map_dir, "/alaska_bathy_raster_EPSG3338.tif"))
  }

  # if user requests contours instead of full bathy, produce these
  if (!is.null(contours)) {
    # check: make sure it is a numeric object
    if (!is.numeric(contours)){
      stop('Enter the contours you want, as in c(200,300). Options are: 50, 100, 200, 400, 500, 600, 800, 1000.')
    }

    #if the contours are provided as negative values, set as positive
    contours <- ifelse(contours > 0, contours, -contours)

    #open the contours file
    bathy_contours <- sf::st_read(paste0(map_dir, '/alaska_bathy_contours_EPSG3338.gpkg'), quiet = TRUE)

    # limit to the requested contour values
    bathy_contours <- bathy_contours[bathy_contours$METERS %in% contours,]

  }

  # limit the extent of the plot to be slightly greater than the plot area (do this in projected space!)
  # this allows us to clip the exent of background layers for plotting
  if (input_crs != "EPSG:3338"){

    region_zoom_box <- sf::st_as_sfc(sf::st_bbox(sf::st_transform(plot_limits_data, crs = 3338)))

  }

  if (input_crs == "EPSG:3338"){

    region_zoom_box <- sf::st_as_sfc(sf::st_bbox(plot_limits_data))

  }

  # create a small buffer, around 10% of the total x-axis extent
  p_min <- sf::st_point(c(min(sf::st_coordinates(region_zoom_box)[, 1]), min(sf::st_coordinates(region_zoom_box)[, 2])))
  p_max <- sf::st_point(c(max(sf::st_coordinates(region_zoom_box)[, 1]), max(sf::st_coordinates(region_zoom_box)[, 2])))

  # compute the maximum distance across plot; add a buffer to the plot as n% of this distance
  dist_buffer <- sf::st_distance(p_min, p_max)[[1]] * plot_expansion

  # apply the buffer
  if (!sf::st_is_longlat(region_zoom_box)) {
    region_zoom_box <- sf::st_buffer(region_zoom_box, dist = dist_buffer, joinStyle = "MITRE", mitreLimit = 2)
  }

  # crop the base layers
  ak_land <- sf::st_intersection(sf::st_geometry(ak_land), sf::st_geometry(region_zoom_box))
  russia_land <- sf::st_intersection(sf::st_geometry(russia_land), sf::st_geometry(region_zoom_box))
  canada_land <- sf::st_intersection(sf::st_geometry(canada_land), sf::st_geometry(region_zoom_box))

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

  if (!is.null(SSL_no_transit)) {
    SSL_no_transit_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(SSL_no_transit_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(humpback_critical_habitat)) {
    humpback_critical_habitat_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(humpback_critical_habitat_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(NPRW_critical_habitat)) {
    NPRW_critical_habitat_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(NPRW_critical_habitat_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(sea_otter_critical_habitat)) {
    sea_otter_critical_habitat_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(sea_otter_critical_habitat_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(CI_beluga_critical_habitat)) {
    CI_beluga_critical_habitat_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(CI_beluga_critical_habitat_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(walrus_protection_area)) {
    walrus_protection_area_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(walrus_protection_area_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(walrus_no_transit)) {
    walrus_no_transit_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(walrus_no_transit_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(NWR_Afognak_Semidi_boundaries)) {
    NWR_Afognak_Semidi_boundaries_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(NWR_Afognak_Semidi_boundaries_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(ringed_seal_critical_habitat)) {
    ringed_seal_critical_habitat_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(ringed_seal_critical_habitat_layer)),
      sf::st_geometry(region_zoom_box)
    )
  }

  if (!is.null(bearded_seal_critical_habitat)) {
    bearded_seal_critical_habitat_layer <- sf::st_intersection(
      sf::st_make_valid(sf::st_geometry(bearded_seal_critical_habitat_layer)),
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
    # bathy_contours <- sf::st_intersection(
    #   sf::st_make_valid(sf::st_geometry(bathy_contours)),
    #   sf::st_geometry(region_zoom_box)
    # )
    sf::st_agr(bathy_contours) = "constant"
    bathy_contours <- sf::st_crop(bathy_contours, region_zoom_box)
  }

  # if the input data (plot_limits_data parameter is not in EPSG:3338, reproject into the input projection)
  if (input_crs != "EPSG:3338"){

    # transform projections on all the layers we need
    ak_land <- sf::st_transform(ak_land, crs = input_crs)
    russia_land <- sf::st_transform(russia_land, crs = input_crs)
    canada_land <- sf::st_transform(canada_land, crs = input_crs)

    if (!is.null(management_regions)) {
      management_regions_layer <- sf::st_transform(management_regions_layer, crs = input_crs)
    }

    if (!is.null(SSL_critical_habitat)) {
      SSL_critical_habitat_layer <- sf::st_transform(SSL_critical_habitat_layer, crs = input_crs)
    }

    if (!is.null(SSL_no_transit)) {
      SSL_no_transit_layer <-  sf::st_transform(SSL_no_transit_layer, crs = input_crs)
    }

    if (!is.null(humpback_critical_habitat)) {
      humpback_critical_habitat_layer <- sf::st_transform(humpback_critical_habitat_layer, crs = input_crs)
    }

    if (!is.null(NPRW_critical_habitat)) {
      NPRW_critical_habitat_layer <- sf::st_transform(NPRW_critical_habitat_layer, crs = input_crs)
    }

    if (!is.null(sea_otter_critical_habitat)) {
      sea_otter_critical_habitat_layer <- sf::st_transform(sea_otter_critical_habitat_layer, crs = input_crs)
    }

    if (!is.null(CI_beluga_critical_habitat)) {
      CI_beluga_critical_habitat_layer <- sf::st_transform(CI_beluga_critical_habitat_layer, crs = input_crs)
    }

    if (!is.null(walrus_no_transit)) {
      walrus_no_transit_layer <- sf::st_transform(walrus_no_transit_layer, crs = input_crs)
    }

    if (!is.null(walrus_protection_area)) {
      walrus_protection_area_layer <- sf::st_transform(walrus_protection_area_layer, crs = input_crs)
    }

    if (!is.null(NWR_Afognak_Semidi_boundaries)) {
      NWR_Afognak_Semidi_boundaries_layer <- sf::st_transform(NWR_Afognak_Semidi_boundaries_layer, crs = input_crs)
    }

    if (!is.null(ringed_seal_critical_habitat)) {
      ringed_seal_critical_habitat_layer <- sf::st_transform(ringed_seal_critical_habitat_layer, crs = input_crs)
    }

    if (!is.null(bearded_seal_critical_habitat)) {
      bearded_seal_critical_habitat_layer <- sf::st_transform(bearded_seal_critical_habitat_layer, crs = input_crs)
    }

    if (!is.null(alaska_3nmi_buffer)) {
      alaska_3nmi_buffer_layer <- sf::st_transform(alaska_3nmi_buffer_layer, crs = input_crs)
    }

    if (bathy == TRUE) {

      # for now, we're not allowing rasters in geographic space! warn:
      warning(paste0('Because you are working with Lat/Lon coordinates (using ', input_crs, '), bathymetric rasters are not an option! Please use a projected coordinate system if you want that! Try EPSG:3338 for Alaska.' ))
    }

    if (!is.null(contours)) {
      bathy_contours <- sf::st_transform(bathy_contours, crs = input_crs)
    }

  }

  #############
  # finally, plot it all

  # get the automatically-generated graticule for the biggest shapefile (ak_land)- we will use this to force more longitude ticks on the basemap if there are an insufficient (<3) number of ticks.
  grats <- sf::st_graticule(ak_land)

  # define a color for the added layers (management regions, etc): white if bathy, black if contours
  layer_col <- ifelse(bathy == TRUE, "white", "black")

  # if a user picked lat/lon, set as black either way (there's no bathymetric option)
  if (isTRUE(sf::st_is_longlat(input_crs))){
    layer_col <- 'black'
  }

  # define the basemap ggplot object
  basemap_layers <- ggplot2::ggplot() +
    {
      if (isTRUE(bathy) & !isTRUE(sf::st_is_longlat(input_crs))) ggplot2::geom_raster(data = bathy_raster_df, ggplot2::aes(x = x, y = y, fill = z))
    } +
    {
      if (bathy == TRUE) bathy_colors
    } +
    {
      if (!is.null(contours) & bathy != TRUE & contour_linescale == FALSE) ggplot2::geom_sf(data = bathy_contours, color = "gray60")
    } +
    {
      if (!is.null(contours) & bathy != TRUE & contour_linescale == TRUE) ggplot2::geom_sf(data = bathy_contours, ggplot2::aes(linetype = factor(METERS)))
    } +
    # add requested critical habitat/ no transit zone layers
    {
      if (!is.null(SSL_critical_habitat)) ggplot2::geom_sf(data = SSL_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    {
    if (!is.null(SSL_no_transit)) ggplot2::geom_sf(data = SSL_no_transit_layer, color = layer_col, fill = "transparent")
} +
    {
      if (!is.null(humpback_critical_habitat)) ggplot2::geom_sf(data = humpback_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(NPRW_critical_habitat)) ggplot2::geom_sf(data = NPRW_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(sea_otter_critical_habitat)) ggplot2::geom_sf(data = sea_otter_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(CI_beluga_critical_habitat)) ggplot2::geom_sf(data = CI_beluga_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(walrus_protection_area)) ggplot2::geom_sf(data = walrus_protection_area_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(walrus_no_transit)) ggplot2::geom_sf(data = walrus_no_transit_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(NWR_Afognak_Semidi_boundaries)) ggplot2::geom_sf(data = NWR_Afognak_Semidi_boundaries_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(ringed_seal_critical_habitat)) ggplot2::geom_sf(data = ringed_seal_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    {
      if (!is.null(bearded_seal_critical_habitat)) ggplot2::geom_sf(data = bearded_seal_critical_habitat_layer, color = layer_col, fill = "transparent")
    } +
    # add requested management layers
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
    {
      if (!is.null(contours) & bathy != TRUE) ggplot2::guides(linetype = 'none')
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
