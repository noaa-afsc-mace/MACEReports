#' @title An example dataset consisting of pollock length, weight, and age data from Gulf of Alasks summer MACE surveys
#' @keywords internal
#' @format A data frame with 19061 rows and 9 columns:
#' \describe{
#'   \item{SURVEY}{A unique identifier for each survey (numeric)}
#'   \item{HAUL}{An identifier for the trawl number that a specimen was captured in}
#'   \item{SPECIES_CODE}{An identifier for the species code for a given specimen}
#'   \item{SUBCATEGORY}{An identifier for the life history stage for a given specimen}
#'   \item{FORK_LENGTH}{pollock length (cm; numeric)}
#'   \item{ORGANISM_WEIGHT}{pollock weight (kg; numeric)}
#'   \item{AGE}{Pollock age (if available) (years; numeric)}
#'   \item{REPORT_NUMBER}{An identifier for the survey region in which a given specimen was captured (numeric)}
#'   \item{region}{An description of the survey region in which a given specimen was captured (character)}
#' }
#' @source Historic MACE reported values
"pollock_length_weight_age_data"
