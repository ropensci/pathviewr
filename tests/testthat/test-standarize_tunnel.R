## Tests of standardize_tunnel() are in this file

test_that("standardize_tunnel() fails when non-numerics are supplied", {
  expect_error(standardize_tunnel("steve"))
  expect_error(standardize_tunnel(c("a", "b", "c")))
  expect_error(standardize_tunnel())
  expect_error(standardize_tunnel(data.frame(rnorm(100))))
})

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Clean the file. It is generally recommended to clean up to the
## "trimmed" step before running standardize_tunnel().
motive_trimmed <-
  motive_data %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers()

## Now rotate the tunnel using default values
motive_rotated <-
  motive_trimmed %>%
  ## pretend subject 2 is perch1 and subject 3 is perch2 just for fun
  standardize_tunnel(landmark_one = "device02",
                     landmark_two = "device03")

## Test input
test_that("standardize_tunnel() handles arguments properly",{
  expect_error(standardize_tunnel(motive_trimmed[,-4]))
  expect_error(standardize_tunnel(motive_trimmed[,-5]))
  expect_error(standardize_tunnel(motive_trimmed[,-6]))
  expect_error(standardize_tunnel(motive_trimmed, landmark_one = "bob"))
  expect_error(standardize_tunnel(motive_trimmed,
                                  landmark_one = "device02",
                                  landmark_two = "bob"))
})
