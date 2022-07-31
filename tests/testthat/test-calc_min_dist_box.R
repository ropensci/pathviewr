## Import flydra data
flydra_data <-
  read_flydra_mat(
    system.file("extdata", "pathviewr_flydra_example_data.mat",
                package = 'pathviewr'),
    subject_name = "birdie_wooster")

## Clean flydra data
flydra_full <-
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
    desired_percent = 60,
    rename_viewr_characters = FALSE,
    separate_trajectories = TRUE,
    get_full_trajectories = TRUE
  )

## Prep flydra data
flydra_test <-
  flydra_full %>%
  insert_treatments(tunnel_config = "box",
                    tunnel_width = 1,
                    tunnel_length = 3,
                    stim_param_lat_pos = 0.05,
                    stim_param_lat_neg = 0.05,
                    stim_param_end_pos = 0.1,
                    stim_param_end_neg = 0.1,
                    treatment = "latB")

flydra_min_dist <-
  flydra_test %>%
  calc_min_dist_box()

test_that("calc_min_dist_box() fails when nonsense is supplied", {
  expect_error(calc_min_dist_box("steve"))
  expect_error(calc_min_dist_box(c("a", "b", "c")))
  expect_error(calc_min_dist_box())
  #expect_error(calc_min_dist_box(flydra_full)) ## no insert treatments
  expect_error(calc_min_dist_box(data.frame(rnorm(100))))
})


## Test output data frame
test_that("calc_min_dist_box() adds variables appropriately",{
  # output has correct variable names
  expect_equal(names(flydra_min_dist)[28:30],
               c("min_dist_pos", "min_dist_neg", "min_dist_end")
  )
  # output has correct dimensions
  expect_equal(dim(flydra_min_dist), c(381, 30)
  )
})


# Test calculations
test_that(
  "get_vis_angle() makes correct calculations based on position_width", {
    # correct visual angles result
    expect_equal(flydra_min_dist$min_dist_pos[1:5],
                 c(0.4936291, 0.4981546, 0.5014801, 0.5048294, 0.5087834),
                 tolerance = 1e-5
    )
    expect_equal(flydra_min_dist$min_dist_neg[1:5],
                 c(0.5063709, 0.5018454, 0.4985199, 0.4951706, 0.4912166),
                 tolerance = 1e-5
    )
    expect_equal(flydra_min_dist$min_dist_end[1:5],
                 c(2.420628, 2.402764, 2.384981, 2.367326, 2.349635),
                 tolerance = 1e-5
    )
  })

