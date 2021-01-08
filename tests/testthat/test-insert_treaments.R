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
motive_treat <-
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

## Import flydra data
flydra_data <-
  read_flydra_mat(
    system.file("extdata", "pathviewR_flydra_example_data.mat",
                package = 'pathviewR'),
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
flydra_treat <-
  flydra_full %>%
  insert_treatments(tunnel_config = "box",
                    tunnel_width = 1,
                    tunnel_length = 3,
                    stim_param_lat_pos = 0.05,
                    stim_param_lat_neg = 0.05,
                    stim_param_end_pos = 0.1,
                    stim_param_end_neg = 0.1,
                    treatment = "latB")

test_that("insert_treatments() fails when nonsense is supplied", {
  expect_error(insert_treatments("steve"))
  expect_error(insert_treatments(c("a", "b", "c")))
  expect_error(insert_treatments())
  expect_error(insert_treatments(flydra_treat,
                                 tunnel_config = "box",
                                 tunnel_width = -1, # negative tunnel width
                                 tunnel_length = 3,
                                 stim_param_lat_pos = 0.05,
                                 stim_param_lat_neg = 0.05,
                                 stim_param_end_pos = 0.1,
                                 stim_param_end_neg = 0.1,
                                 treatment = "latB"))
  expect_error(insert_treatments(flydra_treat,
                                 tunnel_config = "box",
                                 tunnel_width = 1,
                                 tunnel_length = -3, # negative tunnel length
                                 stim_param_lat_pos = 0.05,
                                 stim_param_lat_neg = 0.05,
                                 stim_param_end_pos = 0.1,
                                 stim_param_end_neg = 0.1,
                                 treatment = "latB"))
  expect_error(insert_treatments(flydra_treat,
                                 tunnel_config = "box",
                                 tunnel_width = 1,
                                 tunnel_length = 3,
                                 stim_param_lat_pos = -0.05, # negative stim_param
                                 stim_param_lat_neg = 0.05,
                                 stim_param_end_pos = 0.1,
                                 stim_param_end_neg = 0.1,
                                 treatment = "latB"))

})


## Test output objects
test_that("insert_treatments() adds variables appropriately", {
  ## Inserted variables at beginning of df
  expect_equal(names(motive_treat)[1:9],
               c("tunnel_config", "perch_2_vertex", "vertex_angle",
                 "tunnel_length", "stim_param_lat_pos", "stim_param_lat_neg",
                 "stim_param_end_pos", "stim_param_end_neg", "treatment")
               )
  expect_equal(names(flydra_treat)[1:8],
               c("tunnel_config", "tunnel_width", "tunnel_length",
                 "stim_param_lat_pos", "stim_param_lat_neg",
                 "stim_param_end_pos", "stim_param_end_neg", "treatment")
               )
  ## output objects are of correct dimensions
  expect_equal(dim(motive_treat), c(449, 33))
  expect_equal(dim(flydra_treat), c(381, 23))
})
