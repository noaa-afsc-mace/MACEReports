#' @title Return predicted biomass values that can be plotted in interpolated abundance and distribution maps
#' @description Accepts xyz data (for example, longitude/latitude/abundance) and returns a dataframe with interpolated values. The number of interpolated values returned depends on the chosen map resolution.These objects can be plotted using  \code{gpplot2 geom_raster()}, base \code{plot}, or converted to rasters.
#' @param x Longitude (decimal degrees)
#' @param y Latitude (decimal degrees)
#' @param z Quantity (abundance, biomass, temperature, etc)
#' @param resolution Distance (meters) for interpolation points. Recommended values are 1000 in the Shelikof, 2500 in summer surveys.
#' @param region Region in which to make the interpolations. Currently, the only options are 'shelikof', 'summer_goa', 'core_ebs', and 'sca'.
#' @param out_crs The Coordinate Reference Setting for the returned interpolation points; default is "EPSG:3338"
#' @param interp_type Interpolation formula to apply. Options are 'universal', 'ordinary', 'simple', 'idw'. Universal uses universal kridging with Latitude and Longitude as variables; Ordinary uses ordinary kridging, simple uses simple kriging with the mean set as mean(z), idw performs inverse-distance weighting interpolation. Universal kridging is comprehensive, but very slow. IDW is very coarse, but very fast.
#' @param interp_scale by default, all interpolation is done on log10-transformed quantities (this works well for abundance and biomass). However, you can also interpolate in the linear domain (this works well for temperature values). Options are 'log10' (default) and 'linear'. If you use linear, we recommend only using \code{interp_type = 'idw'} or \code{interp_type = 'simple'}.
#' @param neighborhood (optional) number of nearest observations to use in universal or ordinary kridging calculations. The default is to use all observations; limiting calculations to a neighborhood can be used to speed up the interpolation. This argument passes the number of nearest arguments to use to  \code{Gstat::kridge}; see \code{Gstat::kridge nmax} argument for details.
#' @return a dataframe with 4 columns:
#' x = Longitude (crs specified in out_crs; default default is "EPSG:3338")
#' y = Latitude (crs specified in out_crs; default default is "EPSG:3338")
#' z = abundance value. Note that these values are by default Log 10-transformed abundance, as these tend to highlight patterns in abundance and distribution in MACE datasets. Set \code{interp_scale = 'linear'} if you'd like to return values in the linear domain.
#' @author Mike Levine
#'
#' @examples
#' \dontrun{
#' preds_vals <- get_interpolated_plot_vals(
#'   x = plot_data$START_LONGITUDE,
#'   y = plot_data$START_LATITUDE, z = plot_data$BIOMASS,
#'   resolution = 2500, region = "summer_goa", interp_type = "universal"
#' )
#' }
#' @export
get_interpolated_plot_vals <- function(x,
                                       y,
                                       z,
                                       resolution,
                                       region,
                                       out_crs = "EPSG:3338",
                                       interp_type,
                                       interp_scale = "log10",
                                       neighborhood = NULL) {
  # data checks
  # x,y,z must be numeric
  if (!is.numeric(c(x, y, z))) stop("x,y, and z must all be numeric")

  # do a rough check to ensure the points are lat_decimal degree and lon_decimal degree
  if (max(x) > 1000) stop("x should be Longitude (decimal degrees)")
  if (max(y) > 1000) stop("y should be Latitude (decimal degrees)")

  # region must be specified, and there are four two options right now!
  if (!(region %in% c("shelikof", "summer_goa", "core_ebs", "sca"))) {
    stop(paste0("region must be one of: shelikof, summer_goa, core_ebs, sca, not ", region))
  }

  # interp_type must be specifed as one of the available options
  if (!(interp_type %in% c("universal", "ordinary", "idw", "simple"))) {
    stop(paste0("interp_type must be one of: universal, ordinary, idw, not ", interp_type))
  }

  # if specified, the neighborhood argument must be numeric
  if (!is.null(neighborhood) & !is.numeric(neighborhood)) {
    stop(paste0("neighborhood must a be a numeric value, not a ", class(neighborhood)))
  }

  # interp_scale can only be 'log10' or 'linear'
  if (!(interp_scale %in% c("log10", "linear"))) {
    stop(paste0("interp_scale must be 'log10' or 'linear', not ", interp_scale))
  }

  #####
  # step 1: define the extrapolation grid

  # identify the extrapolation grid to open
  base_dir <- system.file("extdata/EPSG3338/", package = "MACEReports")

  if (region == "shelikof") {
    file_loc <- paste0(base_dir, "/shelikof_extrap_polygon_EPSG3338.gpkg")
  }

  if (region == "summer_goa") {
    file_loc <- paste0(base_dir, "/goa_summer_extrap_polygon_EPSG3338.gpkg")
  }

  if (region == "core_ebs") {
    file_loc <- paste0(base_dir, "/core_ebs_extrp_polygon_EPSG3338.gpkg")
  }

  if (region == "sca") {
    file_loc <- paste0(base_dir, "/sca_extrp_polygon_EPSG3338.gpkg")
  }

  # open up the grid
  extrap_poly <- sf::read_sf(file_loc)

  # define a grid cellsize (units in m from edge-edge of a square)
  extrap_grid <- sf::st_make_grid(extrap_poly, cellsize = resolution, square = TRUE, what = "corners")

  # limit to what's in the extrapolation polygon
  extrap_grid <- extrap_grid[extrap_poly]

  # convert back to sf object; this results in 14k sample points
  extrap_grid <- sf::st_as_sf(extrap_grid)

  # add a lat/lon as variables for interpolation
  extrap_grid <- extrap_grid %>%
    dplyr::mutate(
      Lon = unlist(purrr::map(extrap_grid$x, 1)),
      Lat = unlist(purrr::map(extrap_grid$x, 2))
    )

  ######
  # step 2: get the point data

  # gather the vectors of start positions into a dataframe
  plot_pos_df <- data.frame("x" = x, "y" = y, "z" = z)

  # remove any rows with NA's in the position data
  plot_pos <- plot_pos_df[stats::complete.cases(plot_pos_df), ]

  # report any removed rows
  removed_rows <- plot_pos_df[!(stats::complete.cases(plot_pos_df)), ]
  which_removed <- which(rowSums(is.na(plot_pos_df)) > 0)
  if (nrow(removed_rows) > 0) warning(paste(nrow(removed_rows), "row(s) removed due to missing positions"))

  # turn the plot positions into an sf dataframe; they should start as lat dd/lon dd (i.e. EPSG 4326)
  plot_pos <- sf::st_as_sf(plot_pos, coords = c("x", "y"), remove = TRUE, crs = "EPSG:4326")

  # convert to EPSG:3338 unless a user requests something else
  plot_pos <- sf::st_transform(plot_pos, crs = out_crs)

  # there are very occasionally duplicate positions. These cause interpolations
  # to fail (can't estimate same pos->same pos, so only keep unique positions
  plot_pos <- plot_pos %>%
    dplyr::distinct(.data$geometry, .keep_all = TRUE)

  # because we are only predicting within our survey polygon,
  # we need to limit all surveys to this extent (some surveys extend beyond this)
  plot_pos <- plot_pos[extrap_poly, ]

  # add lat/long information as variables for interpolation
  plot_pos$Lon <- unlist(purrr::map(plot_pos$geometry, 1))
  plot_pos$Lat <- unlist(purrr::map(plot_pos$geometry, 2))

  ############
  # 3. fit variogram, do interpolation

  # define formula based on requested transformation
  if (interp_scale == "log10") {
    formula_spec <- stats::formula(log10(z + 1) ~ Lon + Lat)
  }

  if (interp_scale == "linear") {
    formula_spec <- stats::formula(z ~ Lon + Lat)
  }

  # default interpolation is universal kridging
  if (interp_type == "universal") {
    # warn if linear scale is being used
    if (interp_scale == "linear") {
      warning(paste0(interp_type, " is not recommended! Try interp_type = 'idw' or interp_type = 'simple' instead!"))
    }

    # fit the variogram to the log10 transformed abundance value
    map_variogram <- gstat::variogram(formula_spec, plot_pos)

    # fit the variogram
    map_fit <- gstat::fit.variogram(map_variogram, model = gstat::vgm("Sph"))

    # Krige the data according to the variogram: estimate at the sample grid points:
    # alert user as it is slow
    print(paste0(
      "Interpolating using universal kridging; this can be very slow! Consider a higher resolution value if ",
      "this is taking forever (recommended resolution for summer surveys >= 2500; for winter >= 1000)."
    ))

    # if no limit to the number of observations (neighborhood) is specified, use univeral kridging with all locations
    if (is.null(neighborhood)) {
      # fit the variogram to the log10 transformed abundance value
      map_points <- gstat::krige(
        formula = formula_spec,
        locations = plot_pos, newdata = extrap_grid, model = map_fit,
        # to avoid printing status updates, etc into report, set debug.level = 0
        debug.level = 0
      )
    }

    # if a limit to the number of observations (neighborhood) is specified, use univeral kridging with nmax = neighborhood
    if (!is.null(neighborhood)) {
      # create a convex hull polygon around the sample points
      sample_poly <- plot_pos %>%
        dplyr::summarize(geometry = sf::st_combine(.data$geometry)) %>%
        sf::st_convex_hull()

      # get rid of extrapolation points outside of the sampled region
      extrap_grid <- extrap_grid[sample_poly, ]

      # fit the variogram to the log10 transformed abundance value
      map_points <- gstat::krige(
        formula = formula_spec,
        locations = plot_pos, newdata = extrap_grid, model = map_fit,
        # to avoid printing status updates, etc into report, set debug.level = 0
        debug.level = 0,
        nmax = neighborhood
      )
    }
  }

  # if user requests inverse distance weighting, use this
  if (interp_type == "idw") {
    print(paste0(
      "Interpolating using inverse distance weighting; this can be very slow! ",
      "Consider a higher resolution value if this is taking forever (recommended resolution ",
      "for summer surveys >= 2500; for winter >= 1000)."
    ))

    # fit the variogram to the log10 transformed abundance value
    map_fit <- gstat::gstat(formula = z ~ 1, locations = plot_pos)

    # predict points on the extrapolation grid
    map_points <- stats::predict(map_fit, extrap_grid)
  }

  if (interp_type == "ordinary") {
    # warn if linear scale is being used
    if (interp_scale == "linear") {
      warning(paste0(interp_type, " is not recommended! Try interp_type = 'idw' or interp_type = 'simple' instead!"))
    }

    # fit the variogram to the log10 transformed abundance value
    map_variogram <- gstat::variogram(formula_spec, plot_pos)

    # fit the variogram
    map_fit <- gstat::fit.variogram(map_variogram, model = gstat::vgm("Sph"))

    print(paste0(
      "Interpolating using ordinary kriging; this can be very slow! ",
      "Consider a higher resolution value if this is taking forever (recommended resolution ",
      "for summer surveys >= 2500; for winter >= 1000"
    ))

    # if no limit to the number of observations (neighborhood) is specified, use ordinary kridging with all locations
    if (is.null(neighborhood)) {
      # fit the variogram to the log10 transformed abundance value
      map_points <- gstat::krige(
        formula = formula_spec,
        locations = plot_pos, model = map_fit, newdata = extrap_grid
      )
    }

    # if  limit to the number of observations (neighborhood) is specified, use univeral kridging nmax = neighborhood
    if (!is.null(neighborhood)) {
      # we don't want to interpolate outside the actual samples in this case! There might be areas with no data,
      # so we don't want to interpolate from 'no data -> no data'

      # create a convex hull polygon around the sample points
      sample_poly <- plot_pos %>%
        dplyr::summarize(geometry = sf::st_combine(.data$geometry)) %>%
        sf::st_convex_hull()

      # get rid of extrapolation points outside of the sampled region
      extrap_grid <- extrap_grid[sample_poly, ]

      # fit the variogram to the log10 transformed abundance value
      map_points <- gstat::krige(
        formula = formula_spec,
        locations = plot_pos, model = map_fit, newdata = extrap_grid,
        nmax = neighborhood
      )
    }
  }

  if (interp_type == "simple") {
    # fit the variogram to the log10 transformed abundance value
    map_variogram <- gstat::variogram(formula_spec, plot_pos)

    # fit the variogram
    map_fit <- gstat::fit.variogram(map_variogram, model = gstat::vgm("Sph"))

    print(paste0(
      "Interpolating using simple kriging; this can be very slow! ",
      "Consider a higher resolution value if this is taking forever (recommended resolution ",
      "for summer surveys >= 2500; for winter >= 1000"
    ))

    # if no limit to the number of observations (neighborhood) is specified, use ordinary kridging with all locations
    if (is.null(neighborhood)) {
      # fit the variogram to the log10 transformed abundance value
      map_points <- gstat::krige(
        formula = z ~ 1, beta = mean(plot_pos_df$z, na.rm = TRUE),
        locations = plot_pos, model = map_fit, newdata = extrap_grid
      )
    }

    # if  limit to the number of observations (neighborhood) is specified, use univeral kridging nmax = neighborhood
    if (!is.null(neighborhood)) {
      # we don't want to interpolate outside the actual samples in this case! There might be areas with no data,
      # so we don't want to interpolate from 'no data -> no data'

      # create a convex hull polygon around the sample points
      sample_poly <- plot_pos %>%
        dplyr::summarize(geometry = sf::st_combine(.data$geometry)) %>%
        sf::st_convex_hull()

      # get rid of extrapolation points outside of the sampled region
      extrap_grid <- extrap_grid[sample_poly, ]

      # fit the variogram to the log10 transformed abundance value
      map_points <- gstat::krige(
        formula = z ~ 1, beta = mean(plot_pos_df$z, na.rm = TRUE),
        locations = plot_pos, model = map_fit, newdata = extrap_grid,
        nmax = neighborhood
      )
    }
  }

  ########
  # 4. Convert to a raster for plotting

  # Generate a raster layer from the extrapolation polygon
  extrap_rast <- terra::rast(extrap_grid, resolution = c(resolution * 1.01, resolution * 1.01))

  # turn the prediction values into a matrix to map to the raster
  vals_rast <- map_points %>%
    dplyr::mutate(
      x = unlist(purrr::map(map_points$geometry, 1)),
      y = unlist(purrr::map(map_points$geometry, 2)),
      z = map_points$var1.pred
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(.data$x, .data$y, .data$z)

  pos_matrix <- as.matrix(vals_rast[1:2])

  # turn the predictions into a raster
  preds_raster <- terra::rasterize(pos_matrix, extrap_rast, values = vals_rast$z, fun = mean)

  # make sure projections match
  preds_plot <- terra::project(preds_raster, out_crs, method = "bilinear")

  # convert to a dataframe (to plot with ggplot2)
  preds_plot <- terra::as.data.frame(preds_plot, xy = TRUE)

  # rename the third column as "z" for consistent output
  colnames(preds_plot)[3] <- "z"

  ## TODO: return as sf, raster, or points? options?
  # return as a dataframe for now
  return(preds_plot)
}
