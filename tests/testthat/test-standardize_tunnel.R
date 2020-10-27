## Tests of standardize_tunnel() are in this file

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Clean the file. It is generally recommended to clean up to the
## "gather" step before running rescale_tunnel_data().
motive_gathered <-
  motive_data %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data()

test_that("standardize_tunnel() fails when nonsense is supplied", {
  expect_error(standardize_tunnel("steve"))
  expect_error(standardize_tunnel(c("a", "b", "c")))
  expect_error(standardize_tunnel())
  expect_error(standardize_tunnel(data.frame(rnorm(100))))
  expect_error(standardize_tunnel(motive_data))
})
