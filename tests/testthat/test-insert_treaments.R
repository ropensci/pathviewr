## Tests for insert_treatments()
context("insert_treatments()")


## Import the motive and flydra example datasets
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))
motive_full <-
  motive_data %>%
  clean_viewr(desired_percent = 50,
              max_frame_gap = "autodetect",
              span = 0.95)

flydra_data <-
  read_flydra_mat(system.file("extdata", "pathviewR_flydra_example_data.mat",
                package = 'pathviewR'),
    subject_name = "birdie_wooster")

############# unfinished
flydra_full <-
  flydra_data %>%
  trim_tunnel_outliers() %>%
  redefine_tunnel_center() %>%
############# unfinished



## Run insert_treatments() on each
motive_test <-
  motive_full %>%
  insert_treatments(vertex_height  = 0.3855,
                    vertex_angle   = 45,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.2,
                    treatment      = "latB")

flydra_test <-
  flydra_full %>%
  insert_treatments(pos_wall = 0.5,
                    neg_wall = 0.5,
                    front_wall = 1.0,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.1,
                    stim_param_front = 0.2,
                    treatment = "latB")

## Test output object
test_that("insert_treatments() adds variables appropriately", {
  ## Inserted variables at beginning of df
  expect_equal(names(motive_test)[1:5],
               c("vertex_height", "vertex_angle", "stim_param_pos",
                 "stim_param_neg", "treatment"))

  ## output object is of correct dimensions
  expect_equal(dim(motive_test), c(449, 29))
})

## Test error message for incorrect argument combination
test_that("incorrect argument combination triggers error message", {
  expect_error(insert_treatments(motive_full,
                                 vertex_height = 45,
                                 pos_wall = 0.5),
               "V-shaped and box-shaped arguments supplied.")
})



