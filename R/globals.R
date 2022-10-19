#' @name globals
#' @title globals definitions
#' define the 'undefined' variables in the package to silence the devtools::check() warning
#' no visible binding for global variable'...
utils::globalVariables(c("template_df_catch_data_summer_goa", "template_df_specimen_data_summer_goa",
                         "template_df_haul_data", "template_df_specimen_table_summer_goa"))
