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
  calc_min_dist_v() %>%
  get_vis_angle()

## Get spatial frequencies
  motive_sf <-
    motive_test %>%
    get_sf()

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
  calc_min_dist_box() %>%
  get_vis_angle()


flydra_sf <-
  flydra_test %>%
  get_sf()

test_that("get_sf() fails when nonsense is supplied", {
  expect_error(get_sf("steve"))
  expect_error(get_sf(c("a", "b", "c")))
  expect_error(get_sf())
  expect_error(get_sf(flydra_full)) ## no insert treatments
  expect_error(get_sf(data.frame(rnorm(100))))
})

## Test output data frame
test_that("get_sf() adds variables appropriately",{
  # output has correct variable names
  expect_equal(names(flydra_sf[c(37:39)]),
               c("sf_pos", "sf_neg", "sf_end")
  )
  expect_equal(names(motive_sf[c(43:45)]),
               c("sf_pos", "sf_neg", "sf_end")
  )
  # output has correct dimensions
  expect_equal(dim(flydra_sf), c(381,39))
  expect_equal(dim(motive_sf), c(449,45))
  })


# Test calculations
test_that("get_sf makes correct calculations based on position_width",{
  # correct spatial frequency calculations
  expect_equal(flydra_sf$sf_pos[210:214],
               c(0.1030981, 0.1052851, 0.1071032, 0.1712546, 0.1672070),
               tolerance = 1e-5
  )
  expect_equal(flydra_sf$sf_neg[210:214],
               c(0.2463172, 0.2441260, 0.2423045, 0.1781020, 0.1821501),
               tolerance = 1e-5
  )
  expect_equal(flydra_sf$sf_end[210:214],
               c(0.1059672, 0.1032285, 0.1008556, 0.3881719, 0.3856564),
               tolerance = 1e-5
  )
  expect_equal(motive_sf$sf_pos[62:66],
               c(0.20007909, 0.19879082, 0.07236257, 0.07296959, 0.07249681),
               tolerance = 1e-5
  )
  expect_equal(motive_sf$sf_neg[62:66],
               c(0.02711166, 0.02867355, 0.04941639, 0.04824390, 0.04707736),
               tolerance = 1e-5
  )
  expect_equal(motive_sf$sf_end[62:66],
               c(0.06480766, 0.06051370, 0.28681606, 0.28172211, 0.27692245),
               tolerance = 1e-5
  )
})

