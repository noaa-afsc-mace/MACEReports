testthat::test_that("errors", {

  test = c(1,2,5,NA, 0.0001, '4')
  testthat::expect_error(
    table_nums_format(x = test),
    'table_nums_format function requires numeric or logical values, not character values.'
  )
})
