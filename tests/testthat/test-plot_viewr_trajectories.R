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
  vdiffr::expect_doppelganger("multi plot default",
                      plot_viewr_trajectories(motive_full,
                                              plot_axes = c("length", "width"),
                                              multi_plot = TRUE))
  vdiffr::expect_doppelganger("multi plot lh",
                              plot_viewr_trajectories(motive_full,
                                                      plot_axes = c("length", "height"),
                                                      multi_plot = TRUE))
  vdiffr::expect_doppelganger("multi plot wl",
                              plot_viewr_trajectories(motive_full,
                                                      plot_axes = c("width", "length"),
                                                      multi_plot = TRUE))
  vdiffr::expect_doppelganger("multi plot wh",
                              plot_viewr_trajectories(motive_full,
                                                      plot_axes = c("width", "height"),
                                                      multi_plot = TRUE))
  vdiffr::expect_doppelganger("multi plot hl",
                              plot_viewr_trajectories(motive_full,
                                                      plot_axes = c("height", "length"),
                                                      multi_plot = TRUE))
  vdiffr::expect_doppelganger("multi plot hw",
                              plot_viewr_trajectories(motive_full,
                                                      plot_axes = c("height", "width"),
                                                      multi_plot = TRUE))
})
