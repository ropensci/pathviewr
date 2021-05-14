## Import motive data
motive_data <- # import
  read_motive_csv(
    system.file("extdata", "pathviewr_motive_example_data.csv",
                package = 'pathviewr')
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
                    treatment = "latB") %>%
  calc_min_dist_v(simplify_output = FALSE)

## Get visual angles
motive_vis_angle <-
  motive_test %>%
  get_vis_angle()


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
                    treatment = "latB") %>%
  calc_min_dist_box()

  flydra_vis_angle <-
    flydra_test %>%
    get_vis_angle()

test_that("get_vis_angle() fails when nonsense is supplied", {
  expect_error(get_vis_angle("steve"))
  expect_error(get_vis_angle(c("a", "b", "c")))
  expect_error(get_vis_angle())
  expect_error(get_vis_angle(flydra_full)) ## no insert treatments
  expect_error(get_vis_angle(data.frame(rnorm(100))))
})

## Test output data frame
test_that("get_vis_angle() adds variables appropriately",{
  # output has correct variable names
  expect_equal(names(flydra_vis_angle)[31:36],
               c("vis_angle_pos_rad", "vis_angle_neg_rad", "vis_angle_end_rad",
                 "vis_angle_pos_deg", "vis_angle_neg_deg", "vis_angle_end_deg")
  )
  expect_equal(names(motive_vis_angle)[43:48],
               c("vis_angle_pos_rad", "vis_angle_neg_rad", "vis_angle_end_rad",
                 "vis_angle_pos_deg", "vis_angle_neg_deg", "vis_angle_end_deg")
  )
  # output has correct dimensions
  expect_equal(dim(flydra_vis_angle), c(381, 36)
  )
  expect_equal(dim(motive_vis_angle), c(449, 48)
  )
  })

# Test calculations
test_that(
  "get_vis_angle() makes correct calculations based on position_width", {
              # correct visual angles result
  expect_equal(flydra_vis_angle$vis_angle_pos_deg[1:5],
                c(5.798571, 5.745982, 5.707942, 5.670134, 5.626141),
                  tolerance = 1e-5
                )
  expect_equal(flydra_vis_angle$vis_angle_neg_deg[1:5],
               c(5.652901, 5.703794, 5.741779, 5.780551, 5.827000),
               tolerance = 1e-5
  )
  expect_equal(flydra_vis_angle$vis_angle_end_deg[1:5],
               c(2.366643, 2.384234, 2.402006, 2.419914, 2.438129),
               tolerance = 1e-5
  )
  expect_equal(motive_vis_angle$vis_angle_pos_deg[1:5],
               c(8.774155, 8.613625, 8.419272, 8.162377, 8.000238),
               tolerance = 1e-5
  )
  expect_equal(motive_vis_angle$vis_angle_neg_deg[1:5],
                c(17.04904, 16.98702, 17.03791, 17.59600, 17.97708),
                tolerance = 1e-5
  )
  expect_equal(motive_vis_angle$vis_angle_end_deg[1:5],
               c(3.477492, 3.535738, 3.596469, 3.655746, 3.716369),
               tolerance = 1e-5
  )
})

