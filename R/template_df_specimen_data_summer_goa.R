#' template_df_specimen_data_summer_goa
#'
#' A subset of specimen data, with columns and formats required for creating the standard catch tables
#' used in Summer Gulf of Alaska cruise reports
#' @keywords internal
#' @format A data frame with 5 rows and 14 columns:
#' \describe{
#'   \item{SHIP}{ship number (numeric)}
#'   \item{SURVEY}{survey number (numeric)}
#'   \item{HAUL}{haul number; this is the same as EVENT_ID used elsewhere (numeric)}
#'   \item{SPECIES_CODE}{RACE species code (numeric)}
#'   \item{COMMON_NAME}{species common name (character)}
#'   \item{SCIENTIFIC_NAME}{species scientific name (character)}
#'   \item{PARTITION}{net partition for catch (codend, pocketnet, etc; character)}
#'   \item{ORGANISM_WEIGHT}{individual organism weight (numeric)}
#'   \item{SEX}{organism sex (character)}
#'   \item{MATURITY}{maturity stage (character)}
#'   \item{OTOLITH_TAKEN}{1 if otolith collected; 0 otherwise (numeric)}
#'   \item{AGE}{organism age (numeric)}
#'   \item{organism_length}{organism length- note that the length type is not included here (numeric)}
#'   \item{length_type}{length type: FORK_LENGTH, STANDARD_LENGTH, TOTAL_LENGTH, MANTLE_LENGTH, BELL_DIAMETER,
#'   CARAPACE_LENGTH, WING_SPAN, PREANAL_FIN_LENGTH (character)}
#' }
#'
#'  @source Template dateset was gathered using get_specimen_table_data.R function in Summer 2021 GOA Cruise Report
"template_df_specimen_data_summer_goa"
