## Import motive data
motive_data <- # import
  read_motive_csv(
    system.file("extdata", "pathviewR_motive_example_data.csv",
                package = 'pathviewR')
  )
## Clean motive data
motive_full <-
  motive_data %>%
  clean_viewr(
    relabel_viewr_axes = TRUE,
    gather_tunnel_data = TRUE,
    trim_tunnel_outliers = TRUE,
    standardization_option = "rotate_tunnel",
    select_x_percent = TRUE,
    desired_percent = 50,
    rename_viewr_characters = FALSE,
    separate_trajectories = TRUE,
    max_frame_gap = "autodetect",
    get_full_trajectories = TRUE,
    span = 0.95
  )

## Prep motive data
motive_test <-
  motive_full %>%
  insert_treatments(tunnel_config = "v",
                    perch_2_vertex = 0.3855,
                    vertex_angle = 90,
                    tunnel_length = 2,
                    stim_param_lat_pos = 0.05,
                    stim_param_lat_neg = 0.05,
                    stim_param_end_pos = 0.1,
                    stim_param_end_neg = 0.1,
                    treatment = "latB")

motive_min_dist <-
  motive_test %>%
  calc_min_dist_v(simplify_output = FALSE)

test_that("calc_min_dist_v() fails when nonsense is supplied", {
  expect_error(calc_min_dist_v("steve"))
  expect_error(calc_min_dist_v(c("a", "b", "c")))
  expect_error(calc_min_dist_v())
  expect_error(calc_min_dist_v(motive_full)) ## no insert treatments
  expect_error(calc_min_dist_v(data.frame(rnorm(100))))
})


## Test output data frame
test_that("calc_min_dist_v() adds variables appropriately",{
  # output has correct variable names
  expect_equal(names(motive_min_dist)[34:42],
               c("vertical_2_vertex", "vertical_2_screen",
                 "horizontal_2_screen_pos", "horizontal_2_screen_neg",
                 "min_dist_pos", "min_dist_neg", "bound_pos", "bound_neg",
                 "min_dist_end")
  )
  # output has correct dimensions
  expect_equal(dim(motive_min_dist), c(449,42)
  )
})


# Test calculations
test_that(
  "calc_min_dist_v() makes correct calculations based on position_width", {
    # correct visual angles result
    expect_equal(motive_min_dist$min_dist_pos[223:227],
                 c(0.4474490, 0.1525866, 0.1505689, 0.1497356, 0.1481083),
                 tolerance = 1e-5
    )
    expect_equal(motive_min_dist$min_dist_neg[223:227],
                 c(0.1897518, 0.3329440, 0.3330949, 0.3336412, 0.3331090),
                 tolerance = 1e-5
    )
    expect_equal(motive_min_dist$min_dist_end[223:227],
                 c(0.3445865, 1.6564893, 1.6305929, 1.6034372, 1.5770317),
                 tolerance = 1e-5
    )
  })
