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

flydra_vis_angle <-
  flydra_test %>%
  calc_vis_angle_box()

## Test output data frame
test_that("calc_vis_angle_V() adds variables appropriately",{
  # output has correct variable names
  expect_equal(names(flydra_vis_angle)[23:28],
               c("min_dist_pos", "min_dist_neg", "vis_angle_pos_rad",
                 "vis_angle_neg_rad", "vis_angle_pos_deg", "vis_angle_neg_deg")
  )
  # output has correct dimensions
  expect_equal(dim(flydra_vis_angle), c(133, 28))
})

# Test calculations
test_that(
  "calc_vis_angle_box makes correct calculations based on position_width", {
              # min_dist accomodates positive and negative position_widths
  expect_equal(flydra_vis_angle$min_dist_pos[37:42],
                c(0.5481748,0.5317715,0.5143868,0.4972302,0.4798652,0.4626722),
                  tolerance = 1e-5
                )
  expect_equal(flydra_vis_angle$min_dist_neg[37:42],
                c(0.4518252,0.4682285,0.4856132,0.4972302,0.4798652,0.4626722),
                  tolerance = 1e-5
                )
              # correct visual angles result
  expect_equal(flydra_vis_angle$vis_angle_pos_deg[37:42],
                c(10.42326,10.74293,11.10377,11.48438,11.89704,12.33579),
                  tolerance = 1e-5
                )
  expect_equal(flydra_vis_angle$vis_angle_neg_deg[37:42],
               c(12.62957,12.19052,11.75721,11.48438,11.89704,12.33579),
               tolerance = 1e-5
  )
})

