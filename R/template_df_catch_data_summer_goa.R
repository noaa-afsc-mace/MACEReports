#' template_df_catch_data_summer_goa
#'
#' A subset of catch data, with columns and formats required for creating the standard catch tables
#' used in Summer Gulf of Alaska cruise reports
#' @keywords internal
#' @format A data frame with 5 rows and 8 columns:
#' \describe{
#'   \item{EVENT_ID}{Event number (numeric)}
#'   \item{GEAR}{Gear type (character)}
#'   \item{SPECIES_CODE}{RACE species code (numeric)}
#'   \item{SUBCATEGORY}{Species subcategory (character)}
#'   \item{SCIENTIFIC_NAME}{species scientific name (character)}
#'   \item{COMMON_NAME}{species common name (character)}
#'   \item{WEIGHT_IN_HAUL}{species catch weight (kg; numeric)}
#'   \item{NUMBER_IN_HAUL}{species catch number (individuals; numeric)}
#' }
#' @source Template dateset was gathered using get_raw_catch_summary.R function in Summer 2021 GOA Cruise Report
"template_df_catch_data_summer_goa"

