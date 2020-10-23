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
  calc_vis_angle_box() ##error to be fixed

## Test output data frame
test_that("calc_vis_angle_V() adds variables appropriately",{
  # output has correct variable names
  expect_equal(names(motive_vis_angle_full[c(30:41)]),
               c("height_2_vertex" , "height_2_screen", "width_2_screen_pos",
                 "width_2_screen_neg", "min_dist_pos", "min_dist_neg", "bound_pos"
                 ,"bound_neg", "vis_angle_pos_rad", "vis_angle_neg_rad",
                 "vis_angle_pos_deg", "vis_angle_neg_deg")
  )
  # output has correct dimensions
  expect_equal(dim(motive_vis_angle_full), c(449,41))
})

# Test calculations
test_that("calc_vis_angle_box makes correct calculations based on
            position_width", {
              # calc_vis_angle_box handles pos and neg position_widths correctly
              expect_equal($width_2_screen_neg[220:230],
                           c(0.2524517,0.2590141,0.2647242,0.2683496,0.4708539,
                             0.4710674,0.4718399,0.4710872,0.4719066,0.4726183,0.4737730),
                           tolerance = 1e-5
              )
              # correct visual angle calculations
              expect_equal(motive_vis_angle_full$vis_angle_pos_deg[61:64],
                           c(9.916060,9.977103,10.041513,27.247943),
                           tolerance = 1e-5
              )
            })


