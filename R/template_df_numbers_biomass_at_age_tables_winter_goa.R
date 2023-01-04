#' template_df_numbers_biomass_at_age_tables_winter_goa
#'
#' A subset of abundance data, with columns and formats required for creating the standard biomass- and numbers-
#' by age tables used in winter Gulf of Alaska cruise reports. An imput dataframe can have more columns than this,
#' but it must contain these columns!
#' @keywords internal
#' @format A data frame with 5 rows and 6 columns:
#' \describe{
#'   \item{SURVEY}{Survey number (numeric)}
#'   \item{REPORT_NUMBER}{Report number (numeric)}
#'   \item{AGE}{Age (years; numeric)}
#'   \item{BIOMASS}{Biomass (kg; numeric)}
#'   \item{NUMBERS}{Numbers of individuals (numeric)}
#'   \item{region}{Report region associated with REPORT_NUMBER (character)}
#' }
#' @source Template dateset was gathered using get_total_biomass_by_length.R function in winter 2022 GOA Cruise Report
"template_df_numbers_biomass_at_age_tables_winter_goa"

