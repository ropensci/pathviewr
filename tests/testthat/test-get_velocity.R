## Tests of get_velocity() are in this file

test_that("get_velocity() fails when non-numerics are supplied", {
  expect_error(get_velocity("steve"))
  expect_error(get_velocity(c("a", "b", "c")))
})

## Set up for tests of the function value return
## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                             package = 'pathviewr'))

## Clean the file. It is generally recommended to clean up to the
## "standarization" step before running get_velocity().
 motive_cleaned <-
   motive_data %>%
   relabel_viewr_axes() %>%
   gather_tunnel_data() %>%
   trim_tunnel_outliers() %>%
   rotate_tunnel()

## Now compute velocity and add as columns
 motive_velocity_added <-
   motive_cleaned %>%
   get_velocity(add_to_viewr = TRUE)

## Or set add_to_viewr to FALSE for a standalone object
 motive_velocity_standalone <-
   motive_cleaned %>%
   get_velocity(add_to_viewr = TRUE)

## versions with filtering
 motive_velocity_minfilt <-
    motive_cleaned %>%
    get_velocity(velocity_min = 1)

motive_velocity_maxfilt <-
    motive_cleaned %>%
    get_velocity(velocity_max = 3)


test_that("get_velocity() returns the correct value", {
 expect_equal(
   get_velocity(motive_cleaned, add_to_viewr = TRUE)[1, 15]$height_inst_vel,
   0)
 expect_equal(
   get_velocity(motive_cleaned, add_to_viewr = TRUE)[5, 14]$width_inst_vel,
   -0.387,
   tolerance = 1e-3)
})

test_that("get_velocity() filtering works", {
   expect_equal(
      get_velocity(motive_cleaned, velocity_min = 1)[1, 15]$height_inst_vel,
      0.3357, tol = 1e-3)
   expect_equal(
      get_velocity(motive_cleaned, velocity_max = 3)[5, 14]$width_inst_vel,
      -0.3873968,
      tolerance = 1e-3)
})

test_that("get_velocity() fails when velocity_min is non-numeric", {
  expect_error(
    get_velocity(motive_cleaned, add_to_viewr = TRUE, velocity_min = "bob"))
})
test_that("get_velocity() fails when velocity_max is non-numeric", {
  expect_error(
    get_velocity(motive_cleaned, add_to_viewr = TRUE, velocity_max = "bob"))
})


test_that("get_velocity() fails when columns are missing", {
   expect_error(get_velocity(
      motive_cleaned[, 1:3],
      add_to_viewr = TRUE,
      velocity_min = "bob"
   ))
   expect_error(get_velocity(
      motive_cleaned[, 1:4],
      add_to_viewr = TRUE,
      velocity_min = "bob"
   ))
   expect_error(get_velocity(
      motive_cleaned[, 1:5],
      add_to_viewr = TRUE,
      velocity_min = "bob"
   ))
})
