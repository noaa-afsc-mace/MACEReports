#' @title An example dataset consisting of pollock biomass values at length on a per-interval basis.
#' @format A data frame with 126647 rows and 6 columns:
#' \describe{
#'   \item{INTERVAL}{A unique identifier for each 0.5 nmi acoustic interval (numeric)}
#'   \item{LENGTH}{pollock length (cm; numeric)}
#'   \item{START_LATITUDE}{Latitude (decimal degrees; numeric)}
#'   \item{START_LONGITUDE}{Longitude (decimal degrees; numeric)}
#'   \item{BIOMASS}{Pollock biomass in interval, scaled by interval width (kg; numeric)}
#'   \item{BIOMASS_NM2}{Pollock biomass in interval, density (kg/nmi-2; numeric)}
#'   \item{year}{survey year (numeric)}
#' }
#' #' @source Historic MACE reported values
'shelikof_biomass'
