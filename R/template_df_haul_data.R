#' template_df_haul_data
#'
#' A subset of standard MACE haul data, with columns and formats required for creating the standard haul table
#' in cruise reports
#'
#' @format A data frame with 5 rows and 15 columns:
#' \describe{
#'   \item{EVENT_ID}{Event number (numeric)}
#'   \item{GEAR}{Gear type (character)}
#'   \item{EQ_TIME}{The EQ time entered for the event ("POSIXct" "POSIXt" )}
#'   \item{HB_TIME}{The haulback time entered for the event ("POSIXct" "POSIXt" )}
#'   \item{DURATION_MINS}{Time from EQ-HB (numeric)}
#'   \item{EQ_LATITUDE}{Latitude (decimal degrees; numeric)}
#'   \item{EQ_LONGITUDE}{Longitdue (decimal degrees; numeric)}
#'   \item{HEAD_ROPE_DEPTH}{Mean headrope depth during event (numeric)}
#'   \item{BOTTOM_DEPTH}{Mean bottom depth during event (numeric)}
#'   \item{HEAD_ROPE_TEMP}{Mean head rope temperature during events (Deg C; numeric)}
#'   \item{SURFACE_TEMP}{Mean surface temperature during events (Deg C; numeric)}
#'   \item{POLLOCK_WEIGHT}{Walleye pollock catch weight (kg; numeric)}
#'   \item{POLLOCK_NUMBERS}{Walleye pollock catch numbers (individuals; numeric)}
#'   \item{NON_POLLOCK_WEIGHT}{All other species catch weight (kg; numeric)}
#'   \item{region}{Survey region name (character)}#'
#' }
#' @source Template dateset was gathered using get_haul_table_data.R function in Summer 2021 GOA Cruise Report
"template_df_haul_data"
