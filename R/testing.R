#library(MACEReports)
#
# # #this checks that we have all the right dataframes, and they are in the right format
#  survey_RData_file = 'G:/MikeLevine/testing/202104_GOA_summer_data.RData'
# #
# #
# # #open up the .Rdata file provided
#  load(survey_RData_file)

 # #build the table and caption
 # biomass_table_list =
 # build_biomass_at_length_table_summer_goa(biomass_nums_data = current_survey_pollock_biomass_nums)
 #
 # #pull out table and caption from the list
 # biomass_table =  biomass_table_list[[1]]
 # biomass_caption = biomass_table_list[[2]]
 #
 #  #
#  # #get a template dataframe- the actual values we'll need
#  # template_df_numbers_biomass_at_length_tables_summer_goa = biomass_nums_data%>%
#  #   dplyr::group_by(SURVEY, year, REPORT_NUMBER, region, SPECIES_CODE, LENGTH, BIOMASS, NUMBERS)%>%
#  #   dplyr::select(SURVEY, year, REPORT_NUMBER, region, SPECIES_CODE, LENGTH, BIOMASS, NUMBERS)
#  #
#  # template_df_numbers_biomass_at_length_tables_summer_goa = head(template_df_numbers_biomass_at_length_tables_summer_goa,n = 5)
#  #
#  # save(template_df_numbers_biomass_at_length_tables_summer_goa, file = 'data/template_df_numbers_biomass_at_length_tables_summer_goa.rda')
#
# #build the
# biomass_nums_table_list = build_numbers_at_length_table_summer_goa(biomass_nums_data = current_survey_pollock_biomass_nums)
# biomass_nums_table =  biomass_nums_table_list[[1]]
# biomass_nums_caption = biomass_nums_table_list[[2]]
