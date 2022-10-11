testthat::test_that("errors", {
  testthat::expect_error(
    build_ts_relationships_table(ts_relationships_used = c(1, 2)),
    'ts_relationships_used is not a character vector. Specify a character vector!'
  )
})
