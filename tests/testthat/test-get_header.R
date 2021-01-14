## Tests of get_header_viewr() are in this file

test_that("get_header_viewr() fails when nonsense is supplied", {
  expect_error(get_header_viewr("steve"))
  expect_error(get_header_viewr(c("a", "b", "c")))
})

## Set up for tests of the function value return
## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                              package = 'pathviewr'))

## Test that attributes are read properly
test_that("get_header_viewr() parses header info correctly", {
  expect_equal(get_header_viewr(motive_data)[3,1], "Take Notes")
})
