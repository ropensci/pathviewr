#Tests of plot_viewr_trajectories()
context("plot viewr trajectories")

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

motive_full <-
  motive_data %>%
  clean_viewr(desired_percent = 50,
              max_frame_gap = "autodetect",
              span = 0.95)

#test plot output w/vdiffr
#use addins to open shiny app to validate plots
test_that("plot_viewr_trajectories() plot output is OK", {
  vdiffr::expect_doppelganger("viewr trajs multi plot",
                      plot_viewr_trajectories(motive_full, multi_plot = TRUE))
})
