## Tests for calc_vis_angle_V
context("calc_vis_angle_V")


## Import and prepare the motive and flydra example datasets
motive_data <- # import
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))
motive_full <- # clean
  motive_data %>%
  clean_viewr(desired_percent = 50,
              max_frame_gap = "autodetect",
              span = 0.95)

motive_test <- # prep for calculation
  motive_full %>%
  insert_treatments(vertex_height  = 0.3855,
                    vertex_angle   = 45,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.2,
                    treatment      = "latB")

## run calc_vis_angle_V()
motive_vis_angle_full <-
  motive_test %>%
  calc_vis_angle_V(simplify=FALSE)
motive

## Test output object
test_that("calc_vis_angle_V() adds variables appropriately")
# output has correct variable names
expect_equal(names(motve_vis_angle_full[c(30:41)]),
             c("height_2_vertex" , "height_2_screen", "width_2_screen_pos",
               "width_2_screen_neg", "min_dist_pos", "min_dist_neg", "bound_pos"
               ,"bound_neg", "vis_angle_pos_rad", "vis_angle_neg_rad",
               "vis_angle_pos_deg", "vis_angle_neg_deg")
             )
# output has correct dimensions
expect_equal(dim(motive_vis_angle_full), c(449,41))



test_that("calc_vis_angle_V calculates width_2_screen variables correctly")
expect_equal(motive_vis_angle_full$width_2_screen_neg[220:230],
             c(0.2524517, 0.2590141, 0.2647242, 0.268349,6 0.4708539, 0.4710674
               0.4718399, 0.4710872, 0.4719066, 0.4726183, 0.4737730)
             )

\
