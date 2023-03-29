#' @name globals
#' @title globals definitions
#' @keywords internal
#' define the 'undefined' variables in the package to silence the devtools::check() warning
#' no visible binding for global variable'...
utils::globalVariables(c("template_df_catch_data_summer_goa",
                         "template_df_specimen_data_summer_goa",
                         "template_df_haul_data",
                         "template_df_specimen_table_summer_goa",
                         'template_df_numbers_biomass_at_length_tables_summer_goa',
                         'template_df_numbers_biomass_at_age_tables_summer_goa',
                         'template_df_numbers_biomass_at_length_tables_winter_goa',
                         'shelikof_historic_numbers_by_length_1981_to_2007',
                         'shelikof_historic_numbers_by_age_1981_to_2007',
                         'shelikof_historic_biomass_by_age_1981_to_2007',
                         'shelikof_historic_biomass_by_length_1981_to_2007',
                         'area_name',
                         'base_dir',
                         'x',
                         'y',
                         'z'))
