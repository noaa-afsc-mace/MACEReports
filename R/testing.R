#library(MACEReports)

#this checks that we have all the right dataframes, and they are in the right format
survey_RData_file = 'G:/MikeLevine/testing/202104_GOA_summer_data.RData'

#open up the .Rdata file provided
load(survey_RData_file)
#
#
# a = sapply(current_survey_pollock_biomass_nums, class)
#
# #get a dataframe with each row/value
# b = tibble::tibble('item' = names(a), 'data_type' = a)


