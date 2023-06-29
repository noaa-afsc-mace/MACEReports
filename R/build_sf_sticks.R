#' @title Build stick objects from xyz (lat/long/abundance) data for use in stickplots
#' @description Accepts xyz data (for example, longitude/latitude/abundance) and returns a vertical 'stick'
#' at each position. Sticks are returned as \code{sf LINESTRING} objects.
#' These objects can be plotted using \code{ggplot2::geom_sf()} or base \code{plot}.
#'
#' @param x x- position
#' @param y Y- position
#' @param z Abundance value (the value that will be used to scale sticks)
#' @param group_variable If specified, will specify the group for each stick; this can be used
#' to identify sticks by group.
#' @param return_df If specified, will include all columns from the original dataframe in the returned object.
#' Only one of 'group_variable' or 'return_df' should be specified.
#' @param rotation Rotation of the sticks in degrees from the vertical. Default is 5 degrees.
#' 0 = no rotation; positive values rotate bars in a clockwise direction.
#' @param bar_scale The relative size of sticks. Default is ~1/2 of total plot height.
#' @param crs The coordinate reference system (CRS) into which sticks will be projected. By default, objects will be returned using the default CRS (Albers Equal Area Alaska, EPSG 3338). Any other valid projection can be specified. Note that this relies on the \link[sf:st_crs]{sf::st_crs} function- make sure you are using a valid CRS argument.
#'
#' @examples
#' dat <- data.frame(
#'   "x" = c(-152.2, -150.3, -159.4),
#'   "y" = c(55.2, 55.8, 55.6),
#'   "z" = c(7500, 40000, 28000),
#'   "species" = c("a", "a", "b")
#' )
#'
#' # sticks can be plotted with ggplot2::geom_sf(),
#' # and Coordinate Reference System (CRS) conversions are handled by sf::st_crs()
#' library(ggplot2)
#' library(sf)
#'
#' # you have to provide x,y,z at minimum
#' sticks <- build_sf_sticks(x = dat$x, y = dat$y, z = dat$z)
#'
#' # plot with ggplot2 geom_sf()
#' ggplot() +
#'   geom_sf(data = sticks)
#'
#' # or with base r plotting
#' plot(sticks)
#'
#' # the rotation (from 0) can be specified (in degrees from 0-360). If not specified,
#' # sticks will be rotated to 5 degrees
#' sticks <- build_sf_sticks(x = dat$x, y = dat$y, z = dat$z, rotation = 15)
#'
#' ggplot() +
#'   geom_sf(data = sticks)
#'
#' # If a CRS is not specified, none will be assigned; if one is specified,
#' # it will be set using sf st_crs()
#' sticks <- build_sf_sticks(
#'   x = dat$x, y = dat$y, z = dat$z, rotation = 15, crs = 3338,
#'   group_variable = dat$species
#' )
#'
#' ggplot() +
#'   geom_sf(data = sticks, aes(color = species))
#'
#' # sticks are automatically scaled so that the largest abundance value
#' # this can be modified with bar_scale argument
#' sticks <- build_sf_sticks(x = dat$x, y = dat$y, z = dat$z, rotation = -5, bar_scale = 2)
#' sticks2 <- build_sf_sticks(x = dat$x, y = dat$y, z = dat$z, rotation = 5, bar_scale = .5)
#'
#' ggplot() +
#'   geom_sf(data = sticks) +
#'   geom_sf(data = sticks2, color = "red")
#'
#' @export
build_sf_sticks <- function(x,
                            y,
                            z,
                            group_variable = NULL,
                            return_df = NULL,
                            rotation = 5,
                            bar_scale = 0.5,
                            crs = "EPSG:3338") {
  # verify inputs

  # x,y,z must be numeric
  if (!is.numeric(c(x, y, z))) stop("x,y, and z must all be numeric")

  # x, y,z, and, if provided, group_variable must have the same number of observations
  if (!is.null(group_variable)) {
    if (!all(sapply(list(length(x), length(y), length(z), length(group_variable)),
      FUN = identical, length(x)
    ))) {
      stop("x, y, z and group variable must be vectors of the same length.")
    }
  }

  if (is.null(group_variable)) {
    if (!all(sapply(list(length(x), length(y), length(z)),
      FUN = identical, length(x)
    ))) {
      stop("x, y, and z must be vectors of the same length.")
    }
  }

  # gather the vectors of start positions into a dataframe
  plot_pos_df <- data.frame("start_x" = x, "start_y" = y, "z" = z)

  # remove any rows with NA's
  plot_pos <- plot_pos_df[stats::complete.cases(plot_pos_df), ]

  # if any z- values are zero, these can't be plotted (can't divide by zero); remove these
  # plot_pos = plot_pos[plot_pos$z != 0,]

  # if any z- values are zero, these can't be plotted (can't divide by zero); add a meaninglessly small
  # constant so the zero position is plotted, by the bar height is visually zero
  plot_pos$z <- ifelse(plot_pos$z > 0, plot_pos$z, 0.1e-5)

  # report any removed rows
  # removed_rows = plot_pos_df[!(stats::complete.cases(plot_pos_df)) | plot_pos_df$z == 0,]
  # which_removed = which(rowSums(is.na(plot_pos_df)) > 0 | plot_pos_df$z == 0)
  removed_rows <- plot_pos[!(stats::complete.cases(plot_pos)) | plot_pos$z == 0, ]
  which_removed <- which(rowSums(is.na(plot_pos)) > 0 | plot_pos$z == 0)
  if (nrow(removed_rows) > 0) warning(paste(nrow(removed_rows), "row(s) removed due to NAs or z values = 0"))

  # removed_rows = rbind(removed_rows, plot_pos_df[!is.na(plot_pos_df$z) & plot_pos_df$z == 0,])
  # which_removed = c(which_removed, which(!is.na(plot_pos_df$z) & plot_pos_df$z == 0))
  # if (nrow(removed_rows) > 0) warning(paste(nrow(removed_rows), 'row(s) removed due to z values = 0'))

  # set a scaling factor

  # if you only have 1 datapoint, set this plot height as the height of the bar, plus 25%
  if (nrow(plot_pos) == 1) {
    total_plot_height <- abs(plot_pos$start_y) * .01
  }

  # otherwise use the extent of the plot
  if (nrow(plot_pos) > 1) {
    total_plot_height <- abs(max(plot_pos$start_y) - min(plot_pos$start_y))
  }

  max_bar_height <- total_plot_height * bar_scale
  scaling_factor <- max(plot_pos$z) / max_bar_height

  # get the 'ends' of the bars as:
  # x = same as start_x and y = start_y + scaling factor
  plot_pos$end_x <- plot_pos$start_x
  plot_pos$end_y <- plot_pos$start_y + (plot_pos$z / scaling_factor)

  # to rotate first convert degrees to radians
  rot_rad <- rotation * pi / 180

  # calculate rotated x and y end points
  plot_pos$rot_x <- (cos(rot_rad) * (plot_pos$end_x - plot_pos$start_x)) +
    (sin(rot_rad) * (plot_pos$end_y - plot_pos$start_y)) + plot_pos$start_x

  plot_pos$rot_y <- (sin(rot_rad) * (plot_pos$end_x - plot_pos$start_x)) +
    (cos(rot_rad) * (plot_pos$end_y - plot_pos$start_y)) + plot_pos$start_y

  # create sf linestrings
  plot_lines <- plot_pos %>%
    # paste all the starting and ending lats/longs into one string
    dplyr::mutate(
      start_pos = paste0(.data$start_x, " ", .data$start_y),
      end_pos = paste0(.data$rot_x, " ", .data$rot_y),
      pos_num = seq(1:length(.data$start_pos))
    ) %>%
    # organize by starting position, ending position
    tidyr::pivot_longer(
      cols = c(.data$start_pos, .data$end_pos),
      names_to = "type", values_to = "loc"
    ) %>%
    # convert text coordinates back to individual numeric columns
    dplyr::group_by(.data$pos_num) %>%
    tidyr::separate(.data$loc, c("LONG", "LAT"), sep = " ") %>%
    dplyr::mutate(dplyr::across(c(.data$LONG, .data$LAT), as.numeric)) %>%
    # convert to sf points
    sf::st_as_sf(coords = c("LONG", "LAT")) %>%
    # connect the points as linestrings for each datapoint- in this case, each position
    dplyr::group_by(.data$pos_num, z) %>%
    dplyr::summarize() %>%
    sf::st_cast("LINESTRING") %>%
    dplyr::select(-.data$pos_num)

  # assign crs as needed
  if (!is.null(crs)) {
    # set the crs on the bars for lat/long (i.e. WGS84)
    sf::st_crs(plot_lines) <- sf::st_crs("EPSG:4326")
    # and deal with the transformation to Albers Alaska equal area
    plot_lines <- sf::st_transform(plot_lines, crs = crs)
  }

  # if the user requested a grouping variable, also add this back in
  if (!is.null(group_variable) & is.null(return_df)) {
    colname <- deparse(substitute(group_variable))

    if (stringr::str_detect(colname, "\\$")) {
      colname <- stringr::str_split(colname, "\\$")[[1]][2]
    }

    # remove any NA rows from the index
    if (nrow(removed_rows) > 0) {
      plot_lines[[colname]] <- group_variable[-which_removed]
    }

    if (nrow(removed_rows) == 0) {
      plot_lines[[colname]] <- group_variable
    }
  }

  # if user requested the entire data frame, add this back- this will work as no rows have been re-sorted
  # if the user requested all dataframe columns, return these
  if (!is.null(return_df)) {
    # make sure we have a dataframe
    if (!"data.frame" %in% class(return_df)) {
      stop("Your must specify a data frame in return_df: see ?build_sf_sticks.")
    }

    # join the other rows- get rid of remnant 'geometry' column if present
    if ("geometry" %in% colnames(return_df)) {
      return_df <- return_df %>%
        dplyr::select(-.data$geometry)
    }

    # remove any NA rows from the index if needed
    if (nrow(removed_rows) > 0) {
      return_df <- return_df[-which_removed]

      plot_lines <- dplyr::bind_cols(plot_lines, return_df)
    }

    if (nrow(removed_rows) == 0) {
      plot_lines <- dplyr::bind_cols(plot_lines, return_df)
    }
  }

  # return this dataframe
  return(plot_lines)
}
