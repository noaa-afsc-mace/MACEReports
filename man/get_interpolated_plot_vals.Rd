% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_interpolated_plot_vals.R
\name{get_interpolated_plot_vals}
\alias{get_interpolated_plot_vals}
\title{Return predicted biomass values that can be plotted in interpolated abundance and distribution maps}
\usage{
get_interpolated_plot_vals(
  x,
  y,
  z,
  resolution,
  region,
  out_crs = "EPSG:3338",
  interp_type,
  interp_scale = "log10",
  neighborhood = NULL
)
}
\arguments{
\item{x}{Longitude (decimal degrees)}

\item{y}{Latitude (decimal degrees)}

\item{z}{Quantity (abundance, biomass, temperature, etc)}

\item{resolution}{Distance (meters) for interpolation points. Recommended values are 1000 in the Shelikof, 2500 in summer surveys.}

\item{region}{Region in which to make the interpolations. Currently, the only options are 'shelikof', 'summer_goa', 'core_ebs', and 'sca'.}

\item{out_crs}{The Coordinate Reference Setting for the returned interpolation points; default is "EPSG:3338"}

\item{interp_type}{Interpolation formula to apply. Options are 'universal', 'ordinary', 'simple', 'idw'. Universal uses universal kridging with Latitude and Longitude as variables; Ordinary uses ordinary kridging, simple uses simple kriging with the mean set as mean(z), idw performs inverse-distance weighting interpolation. Universal kridging is comprehensive, but very slow. IDW is very coarse, but very fast.}

\item{interp_scale}{by default, all interpolation is done on log10-transformed quantities (this works well for abundance and biomass). However, you can also interpolate in the linear domain (this works well for temperature values). Options are 'log10' (default) and 'linear'. If you use linear, we recommend only using \code{interp_type = 'idw'} or \code{interp_type = 'simple'}.}

\item{neighborhood}{(optional) number of nearest observations to use in universal or ordinary kridging calculations. The default is to use all observations; limiting calculations to a neighborhood can be used to speed up the interpolation. This argument passes the number of nearest arguments to use to  \code{Gstat::kridge}; see \code{Gstat::kridge nmax} argument for details.}
}
\value{
a dataframe with 4 columns:
x = Longitude (crs specified in out_crs; default default is "EPSG:3338")
y = Latitude (crs specified in out_crs; default default is "EPSG:3338")
z = abundance value. Note that these values are by default Log 10-transformed abundance, as these tend to highlight patterns in abundance and distribution in MACE datasets. Set \code{interp_scale = 'linear'} if you'd like to return values in the linear domain.
}
\description{
Accepts xyz data (for example, longitude/latitude/abundance) and returns a dataframe with interpolated values. The number of interpolated values returned depends on the chosen map resolution.These objects can be plotted using  \code{gpplot2 geom_raster()}, base \code{plot}, or converted to rasters.
}
\examples{
\dontrun{
preds_vals <- get_interpolated_plot_vals(
  x = plot_data$START_LONGITUDE,
  y = plot_data$START_LATITUDE, z = plot_data$BIOMASS,
  resolution = 2500, region = "summer_goa", interp_type = "universal"
)
}
}
\author{
Mike Levine
}
