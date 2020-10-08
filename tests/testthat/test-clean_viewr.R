## Tests of clean_viewr()

## Pre-import tests
test_that("clean_viewr() fails when no file is supplied",
          {
            expect_error(clean_viewr())
          })
test_that("clean_viewr() fails when unrecognized arguments are supplied",
          {
            expect_error(clean_viewr(steve = "steve"))
          })


## Import the file for other tests (see below)
motive_test_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))


## Test that true/false checks fail successfully
test_that("clean_viewr() fails when relabel_viewr_axes has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     relabel_viewr_axes = FALSE,
                                     tunnel_length = "_z",
                                     tunnel_width = "_x",
                                     tunnel_height = "_y",
                                     real = "_w"))
          })
test_that("clean_viewr() fails when gather_tunnel_data has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     gather_tunnel_data = FALSE,
                                     NA_drop = TRUE))
          })
test_that("clean_viewr() fails when trim_tunnel_outliers has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     trim_tunnel_outliers = FALSE,
                                     lengths_min = 0,
                                     lengths_max = 3,
                                     widths_min = -0.4,
                                     widths_max = 0.8,
                                     heights_min = -0.2,
                                     heights_max = 0.5))
          })
test_that("clean_viewr() fails when get_velocity has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     get_velocity = FALSE,
                                     time_col = "time_sec",
                                     length_col = "position_length",
                                     width_col = "position_width",
                                     height_col = "position_height"))
          })
test_that("clean_viewr() fails when select_x_percent has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     select_x_percent = FALSE,
                                     desired_percent = 33))
          })
test_that("clean_viewr() fails when rename_viewr_characters has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     rename_viewr_characters = FALSE,
                                     target_column = "subject"))
          })
test_that("clean_viewr() fails when separate_trajectories has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     separate_trajectories = FALSE,
                                     max_frame_gap = 1,
                                     frame_rate_proportion = 0.1))
          })
test_that("clean_viewr() fails when get_full_trajectories has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     get_full_trajectories = FALSE,
                                     span = 0.8))
          })
test_that("clean_viewr() fails when fill_traj_gaps has args but is FALSE",
          {
            expect_error(clean_viewr(motive_test_data,
                                     fill_traj_gaps = FALSE,
                                     loess_degree = 1,
                                     loess_criterion = c("aicc", "gcv"),
                                     loess_family = c("gaussian", "symmetric")))
          })
