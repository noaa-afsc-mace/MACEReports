#' template_df_numbers_biomass_at_length_tables_winter_goa
#'
#' A subset of abundance data, with columns and formats required for creating the standard biomass- and numbers-
#' by length tables used in winter Gulf of Alaska cruise reports.
#' @keywords internal
#' @format A data frame with 5 rows and 7 columns:
#' \describe{
#'   \item{SURVEY}{Survey number (numeric)}
#'   \item{REPORT_NUMBER}{Report number (numeric)}
#'   \item{LENGTH}{Length (cm; numeric)}
#'   \item{BIOMASS}{Biomass (kg; numeric)}
#'   \item{NUMBERS}{Numbers of individuals (numeric)}
#'   \item{region}{Report region associated with REPORT_NUMBER (character)}
#'   \item{year}{Survey year (numeric)}
#' }
#' @source Template dateset was gathered using get_total_biomass_by_length.R function in winter 2022 GOA Cruise Report
"template_df_numbers_biomass_at_length_tables_winter_goa"

