## Tests of read_flydra_mat()

## Pre-import tests
test_that(
  "read_flydra_mat() fails when no file is supplied",
  {
    expect_error(
      read_flydra_mat(subject_name = "birdie_wooster"),
      "A mat_file is required"
    )
  }
)
test_that(
  "read_flydra_mat() stops when no subject is supplied",
  {
    expect_error(
      read_flydra_mat(
        system.file("extdata", "pathviewr_flydra_example_data.mat",
                                  package = 'pathviewr')
        ),
      "A subject_name is required")
})


## Import the file for other tests (see below)
flydra_test_data <-
  read_flydra_mat(system.file("extdata", "pathviewr_flydra_example_data.mat",
                             package = 'pathviewr'),
                  subject_name = "birdie_wooster")


## Test that imported files have the correct structure
test_that("read_flydra_mat() reads data in correctly", {
  ## Check that the column names appear correctly
  expect_equal(names(flydra_test_data), c("frame", "time_sec", "subject",
                                          "position_length", "position_width",
                                          "position_height", "velocity",
                                          "length_inst_vel",
                                          "width_inst_vel", "height_inst_vel"))
  ## Check that all data were imported
  expect_equal(dim(flydra_test_data), c(7744, 10))
})
