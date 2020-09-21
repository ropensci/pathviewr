context("importing flydra mat files")

## Pre-import tests
test_that(
  "read_flydra_mat stops when no subject is supplied",
  {
    expect_error(
      read_flydra_mat(
        system.file("extdata", "pathviewR_flydra_example_data.mat",
                                  package = 'pathviewR')
        ),
      "A subject_name is required")
})

## Import the file for other tests
flydra_test_data <-
  read_flydra_mat(system.file("extdata", "pathviewR_flydra_example_data.mat",
                             package = 'pathviewR'),
                  subject_name = "birdie_wooster")


test_that("read_flydra_mat reads data in correctly", {
  ## Check that the column names appear correctly
  expect_equal(names(flydra_test_data), c("frame", "time_sec", "subject",
                                          "position_length", "position_width",
                                          "position_height"))
  ## Check that all data were imported
  expect_equal(dim(flydra_test_data), c(7744, 6))
})
