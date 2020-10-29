## Import and prepare the motive and flydra example datasets
flydra_data <- # import
  read_flydra_mat(system.file("extdata", "pathviewR_flydra_example_data.mat",
                              package = 'pathviewR'),
                  subject_name = "birdie_wooster")

flydra_full <- # clean
  flydra_data %>%
  clean_viewr(
    relabel_viewr_axes = FALSE,
    gather_tunnel_data = FALSE,
    trim_tunnel_outliers = FALSE,
    standardization_option = "redefine_tunnel_center",
    length_method = "middle",
    height_method = "user-defined",
    height_zero = 1.44,
    get_velocity = FALSE,
    select_x_percent = TRUE,
    rename_viewr_characters = FALSE,
    separate_trajectories = TRUE,
    get_full_trajectories = TRUE
  )

flydra_test <- # prep for calculations
  flydra_full %>%
  insert_treatments(pos_wall = 0.5,
                    neg_wall = 0.5,
                    front_wall = 1.0,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.1,
                    stim_param_front = 0.2,
                    treatment = "latB")

flydra_sf <-
  flydra_test %>%
  calc_sf_box()

test_that("calc_sf_box() fails when nonsense is supplied", {
  expect_error(calc_sf_box("steve"))
  expect_error(calc_sf_box(c("a", "b", "c")))
  expect_error(calc_sf_box())
  expect_error(calc_sf_box(flydra_full)) ## no insert treatments
  expect_error(calc_sf_box(data.frame(rnorm(100))))
})

## Test output data frame
test_that("calc_sf_box() adds variables appropriately",{
  # output has correct variable names
  expect_equal(names(flydra_sf[c(23:26)]),
               c("min_dist_pos", "min_dist_neg", "sf_pos", "sf_neg")
              )
  # output has correct dimensions
  expect_equal(dim(flydra_sf), c(133,26))
})


# Test calculations
test_that("calc_sf_box makes correct calculations based on position_width",{
  # min_dist handles pos and neg position_widths
   expect_equal(flydra_sf$min_dist_pos[48:52],
               c(0.3548123,0.3370267,0.4597915,0.4611327,0.4618872),
               tolerance = 1e-5
  )
   expect_equal(flydra_sf$min_dist_neg[48:52],
               c(0.6451877,0.6629733,0.5402085,0.5388673,0.5381128),
               tolerance = 1e-5
  )
  # correct spatial frequency calculations
  expect_equal(flydra_sf$sf_pos[48:52],
               c(0.1238654,0.1176565,0.1605138,0.1609820,0.1612454),
               tolerance = 1e-5
  )
  expect_equal(flydra_sf$sf_neg[48:52],
               c(0.2252359,0.2314448,0.1885875,0.1881193,0.1878559),
               tolerance = 1e-5
  )
})
