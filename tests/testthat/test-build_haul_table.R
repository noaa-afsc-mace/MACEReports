testthat::test_that("errors", {

  test_dat = template_df_haul_data
  test_dat$NON_POLLOCK_WEIGHT = as.character(test_dat$NON_POLLOCK_WEIGHT)
  testthat::expect_error(
    build_haul_table(haul_data = test_dat),
    'Wrong data type in haul_data: NON_POLLOCK_WEIGHT. Values should be: numeric'
  )
})
