## Tests of gather_tunnel_data() are in this file

test_that("gather_tunnel_data() fails when non-numerics are supplied", {
  expect_error(gather_tunnel_data("steve"))
  expect_error(gather_tunnel_data(c("a", "b", "c")))
  expect_error(gather_tunnel_data())
  expect_error(gather_tunnel_data(data.frame(rnorm(100))))
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

motive_garbage <-
  motive_data %>%
  relabel_viewr_axes() %>%
  dplyr::select(frame, time_sec)

motive_garbage2 <-
  motive_data %>%
  relabel_viewr_axes() %>%
  dplyr::select(frame, time_sec, device02_position_length,
                device02_position_width)

## Test output data frame
test_that("gather_tunnel_data() reorganizes data appropriately",{
  # output has correct variable names
  expect_equal(names(motive_gathered)[3:5],
               c("subject", "position_length", "position_width")
  )
  # output has correct dimensions
  expect_equal(dim(motive_gathered), c(2613, 11))
})

## Test input with missing data
test_that("gather_tunnel_data() reorganizes data appropriately",{
  expect_error(gather_tunnel_data(motive_garbage))
  expect_error(gather_tunnel_data(motive_garbage2))
})
