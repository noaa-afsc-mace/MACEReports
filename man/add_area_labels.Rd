% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_area_labels.R
\name{add_area_labels}
\alias{add_area_labels}
\title{Add area labels to the big collection of labels}
\usage{
add_area_labels(lat_dd, lon_dd, area_name)
}
\arguments{
\item{lat_dd}{Latitude (decimal degree)}

\item{lon_dd}{Longitude (decimal degree)}

\item{area_name}{Area name (character)}
}
\value{
None- adds a single row to the \code{'alaska_area_labels.gpkg'} file.
}
\description{
Adds a label to the collection given latitude, longitude, and a name.
}
\author{
Mike Levine
}
\keyword{internal}
