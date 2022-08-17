
testthat::test_that("errors", {
  testthat::expect_error(
    build_sf_sticks(x = c(1,'2',3), y = c(2,3,4), z =c(8,9,10)),
    'x,y, and z must all be numeric'
  )

  testthat::expect_error(
    build_sf_sticks(x = c(1,2,3), y = c(2,3), z =c(8,9,10)),
    "x, y, and z must be vectors of the same length."
  )

  testthat::expect_error(
    build_sf_sticks(x = c(1,2,3), y = c(2,4), z =c(8,9,10), group_variable = c('a', 'a', 'b')),
    "x, y, z and group variable must be vectors of the same length."
  )

})

testthat::test_that("warnings", {

  testthat::expect_warning(
    build_sf_sticks(x = c(1,NA,3), y = c(2,3,4), z =c(8,9,10), group_variable = c('a', 'a', 'b')),
    'removed due to NAs'
  )

})
