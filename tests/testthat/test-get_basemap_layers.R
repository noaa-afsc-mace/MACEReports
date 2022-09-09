#test data
test_df = data.frame('x' = c(52.57357, 52.58147, 52.58919),
                     'y' = c(-169.1898, -169.1940, -169.1986))

test_sf_no_crs = sf::st_as_sf(test_df, coords = c('x', 'y'))

testthat::test_that("errors", {

  testthat::expect_error(
    get_basemap_layers(plot_limits_data = test_df)
  )

  testthat::expect_error(
        get_basemap_layers(plot_limits_data = test_sf_no_crs)
  )

})
