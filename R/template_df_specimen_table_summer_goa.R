#' template_df_specimen_table_summer_goa
#'
#' A subset of specimen data that is formatted for the standard summer GOA number of specimens/biological samples table.
#' This table reports all the current reporting species- walleye pollock, pacific capelin, and pacific ocean perch-
#' as well as an 'all other species' count
#'
#' #' @format A data frame with 5 rows and 14 columns:
#' \describe{
#'   \item{HAUL}{haul number; this is the same as EVENT_ID used elsewhere (numeric)}
#'   \item{POLLOCK_CATCH_LENGTHS}{number of pollock lengths per haul (numeric)}
#'   \item{POLLOCK_WEIGHTS}{number of pollock weights per haul (numeric)}
#'   \item{MATURITIES}{number of pollock maturities per haul (numeric)}
#'   \item{OTOLITHS}{number of pollock otoliths collected per haul (numeric)}
#'   \item{O_TAKEN}{number of pollock ovaries sampled per haul (numeric)}
#'   \item{CAPELIN_CATCH_LENGTHS}{number of pacific capelin lengths per haul (numeric)}
#'   \item{CAPELIN_WEIGHTS}{number of pacific capelin weights per haul (numeric)}
#'   \item{POP_CATCH_LENGTHS}{number of pacific ocean perch lengths per haul (numeric)}
#'   \item{POP_WEIGHTS}{number of pacific ocean perch weights per haul (numeric)}
#'   \item{OTHER_CATCH_LENGTHS}{number of any other species (NOT pollock/capelin/POP) lengths per haul (numeric)}
#'   \item{OTHER_WEIGHTS}{number of any other species (NOT pollock/capelin/POP) weights per haul (numeric)}
#'   \item{SURVEY}{survey number (numeric)}
#'   \item{region}{Survey region name (character)}
#' }
#' #' @source Template dateset was gathered using get_specimen_table_data.R function in Summer 2021 GOA Cruise Report
'template_df_specimen_table_summer_goa'
