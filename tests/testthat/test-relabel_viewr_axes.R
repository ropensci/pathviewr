## Tests of relabel_viewr_axes() are in this file

test_that("relabel_viewr_axes() fails when nonsense is supplied", {
  expect_error(relabel_viewr_axes("steve"))
  expect_error(relabel_viewr_axes(c("a", "b", "c")))
})

## Set up for tests of the function value return
## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Test that attributes are read properly
test_that(
  "relabel_viewr_axes() fails when character vectors are not supplied", {
  expect_error(relabel_viewr_axes(motive_data, tunnel_length = 5))
  expect_error(relabel_viewr_axes(motive_data, tunnel_width =  5))
  expect_error(relabel_viewr_axes(motive_data, tunnel_height = 5))
})
