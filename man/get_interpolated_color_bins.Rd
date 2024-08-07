% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_interpolated_color_bins.R
\name{get_interpolated_color_bins}
\alias{get_interpolated_color_bins}
\title{Return logical breaks that can be used to plot abundance values}
\usage{
get_interpolated_color_bins(z, n_bins = 9, style = "log10", nice_labels = TRUE)
}
\arguments{
\item{z}{a vector of abundance values}

\item{n_bins}{the number of bins you'd like to return; default is 9. This is ignored if style 'log10'.}

\item{style}{the algorithm used in binning; default is 'log10', which places abundance values in log10 bins; 'kmeans' also produces good results.}

\item{nice_labels}{return label values in linear (vs log) space that are formatted for use on plots; this is meant to be #' used in conjunction with values generated by \code{get_interpolated_plot_vals}, which return log10 units.}
}
\description{
Return binned abundance data for plotting interpolated values with greater contrast. This is a wrapper around
the \link[classInt:classIntervals]{classInt::classIntervals} function to make it easier to return binned color values.
}
\examples{
\dontrun{
# Get some interpolated plot values
shelikof_xyz <- shelikof_biomass \%>\%
  filter(year == 2021) \%>\%
  group_by(INTERVAL, START_LATITUDE, START_LONGITUDE) \%>\%
  summarize(
    BIOMASS = sum(BIOMASS),
    BIOMASS_NM2 = sum(BIOMASS_NM2)
  )

interp_vals_2021 <- get_interpolated_plot_vals(x = shelikof_xyz$START_LONGITUDE,
y = shelikof_xyz$START_LATITUDE,
z = shelikof_xyz$BIOMASS,
resolution = 1000,
region = "shelikof",
interp_type = "universal",
neighborhood = 200)

# create an sf object we can use to define the basemap extent
extent <- st_as_sf(interp_vals_2021, coords = c("x", "y"), crs = "EPSG:3338")

# # Return a simple basemap with contours as the bathymetery seems too busy with interpolated maps
basemap <- get_basemap_layers(plot_limits_data = extent,
bathy = FALSE,
contours = c(100, 200, 1000))

# Get the interpolated values: at a minimum, provide Z values
interp_vals_2021$plot_col <- get_interpolated_color_bins(z = interp_vals_2021$z)

basemap +
  geom_raster(data = interp_vals_2021, aes(x = x, y = y, fill = plot_col)) +
  scale_fill_viridis_d(option = "magma")
}
}
\author{
Mike Levine
}
