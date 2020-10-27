## Tests of rescale_tunnel_data() are in this file

test_that("rescale_tunnel_data() fails when non-numerics are supplied", {
  expect_error(rescale_tunnel_data("steve"))
  expect_error(rescale_tunnel_data(c("a", "b", "c")))
  expect_error(rescale_tunnel_data())
  expect_error(rescale_tunnel_data(data.frame(rnorm(100))))
})

## Set up for tests of the function value return
## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Clean the file up to gather()
motive_gathered <-
  motive_data %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data()

## Test output data frame
test_that("rescale_tunnel_data() rescales data appropriately",{
  # output has correct variable names
  expect_equal(as.numeric(rescale_tunnel_data(motive_gathered)[3,5]),
               0.249, tolerance = 1e-3
  )
  # output has correct dimensions
  expect_equal(
    as.numeric(rescale_tunnel_data(motive_gathered)[3,5])/
      as.numeric(motive_gathered[3,5]), 2,
    tolerance = 1e-2
  )
})

## Test input
test_that("rescale_tunnel_data() rescales data appropriately",{
  expect_error(rescale_tunnel_data(motive_gathered, original_scale = "a"))
  expect_error(rescale_tunnel_data(motive_gathered, desired_scale = "b"))
})
