## Tests of rotate_tunnel() are in this file

test_that("rotate_tunnel() fails when non-numerics are supplied", {
  expect_error(rotate_tunnel("steve"))
  expect_error(rotate_tunnel(c("a", "b", "c")))
  expect_error(rotate_tunnel())
  expect_error(rotate_tunnel(data.frame(rnorm(100))))
})

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Clean the file. It is generally recommended to clean up to the
## "trimmed" step before running rotate_tunnel().
motive_trimmed <-
  motive_data %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers()

## Now rotate the tunnel using default values
motive_rotated <-
  motive_trimmed %>%
  rotate_tunnel()

## Test input
test_that("rotate_tunnel() handles arguments properly",{
  expect_error(rotate_tunnel(motive_trimmed[,-4]))
  expect_error(rotate_tunnel(motive_trimmed[,-5]))
  expect_error(rotate_tunnel(motive_trimmed[,-6]))
})
