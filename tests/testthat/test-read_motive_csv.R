## Tests of read_motive_csv()

## Pre-import tests
test_that("read_motive_csv() fails when no file is supplied",
          {
            expect_error(read_motive_csv())
          })



## Import the file for other tests (see below)
motive_test_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                              package = 'pathviewr'))


## Test that imported files have the correct structure
test_that("read_motive_csv() reads data in correctly", {
  ## Check that the column names appear correctly
  expect_equal(names(motive_test_data)[c(1,2,5)],
               c("frame", "time_sec", "device02_rotation_z"))
  ## Check that all data were imported
  expect_equal(dim(motive_test_data), c(934, 26))
})
