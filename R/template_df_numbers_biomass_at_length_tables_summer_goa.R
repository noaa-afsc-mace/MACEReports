#' template_df_numbers_biomass_at_length_tables_summer_goa
#'
#' A subset of biomass-and-numbers at length data, with columns and formats required for creating the standard catch tables
#' used in Summer Gulf of Alaska cruise reports. Note that the dataframe used to match this template can have MORE columns,
#' but it must at least have the below columns.
#'
#' @format A data frame with 5 rows and 8 columns:
#' \describe{
#'   \item{SURVEY}{survey number (numeric)}
#'   \item{year}{survey year; (YYYY; numeric)}
#'   \item{report_number}{the report number assigned by analysts when running Macebase Analysis (numeric)}
#'   \item{region}{Survey region name (character)}
#'   \item{SPECIES_CODE}{RACE species code (numeric)}
#'   \item{LENGTH}{length (cm; numeric)}
#'   \item{BIOMASS}{biomass-at-length (kg; numeric)}
#'   \item{NUMBERS}{numbers-at-length (individuals, numeric)}
#' }
#'  @source Template dateset was gathered using get_specimen_table_data.R function in Summer 2021 GOA Cruise Report
"template_df_numbers_biomass_at_length_tables_summer_goa"
