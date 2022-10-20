testthat::test_that("errors", {

  test_dat = template_df_numbers_biomass_at_length_tables_summer_goa[,1:7]
  testthat::expect_error(
    build_numbers_at_length_table_summer_goa(biomass_nums_data = test_dat),
    'biomass_nums_data is missing NUMBERS column'
  )

  test_dat = template_df_numbers_biomass_at_length_tables_summer_goa
  test_dat$BIOMASS = as.character(test_dat$BIOMASS)
  testthat::expect_error(
    build_numbers_at_length_table_summer_goa(biomass_nums_data = test_dat),
    'Wrong data type in biomass_nums_data: BIOMASS. Values should be: numeric'
  )

  test_dat = template_df_numbers_biomass_at_length_tables_summer_goa
  test_dat$SURVEY[1] = 202207
  testthat::expect_error(
    build_numbers_at_length_table_summer_goa(biomass_nums_data = test_dat),
    'biomass_nums_data contains multiple species surveys: "202207", "202104"'
  )
})

testthat::test_that("warnings", {

  test_dat = template_df_numbers_biomass_at_length_tables_summer_goa
  test_dat$SPECIES_CODE = 21110

  testthat::expect_warning(
    build_numbers_at_length_table_summer_goa(biomass_nums_data = test_dat),
    'You are printing a table for species code 21110 not walleye pollock! Is this okay'
  )

})
