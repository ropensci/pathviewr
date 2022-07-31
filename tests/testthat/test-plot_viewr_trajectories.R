#Tests of plot_viewr_trajectories()

## test of object input
test_that(
  "plot_viewr_trajectories() fails when data are missing or nonsense args",
          {
            expect_error(plot_viewr_trajectories(multi_plot = "steve"))
            expect_error(plot_viewr_trajectories(plot_axes = 1))
            expect_error(plot_viewr_trajectories(obj_name = "jobin"))
            expect_error(plot_viewr_trajectories(rnorm(100)))
            expect_error(plot_viewr_trajectories())
          })

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                              package = 'pathviewr'))

motive_full <-
  motive_data %>%
  clean_viewr(desired_percent = 50,
              max_frame_gap = "autodetect",
              span = 0.95)

#pre plot data wrangle
obj_name_trajs <- unique(motive_full$file_sub_traj)
sqrt_traj_count <- ceiling(sqrt(length(obj_name_trajs)))

test_that("wrangle for plots OK", {
  expect_match(
    unique(motive_full$file_sub_traj)[[6]],
    "pathviewr_motive_example_data.csv_device03_0"
  )
  expect_equal(ceiling(sqrt(length(obj_name_trajs))), 4)
})

i <- length(obj_name_trajs)
tmp <-
  motive_full %>% dplyr::filter(file_sub_traj == obj_name_trajs[i])

#aspects of plot and loop
test_that("aspects of plotting loop are functioning as expected", {
  expect_equal(i, 10)
  expect_match(tmp[[14, 3]], "device05")
  expect_equal(tmp$traj_id[[20]], 2)
  expect_equal(tmp[[7, 1]], 105069)
})
test_that("plot info (axes, titles, etc.) is correct", {
  expect_match(obj_name_trajs[9],
               "pathviewr_motive_example_data.csv_device05_1")
  expect_match(paste0("trajectory #", 5), "trajectory #5")
  expect_equal(min(motive_full$position_length), -0.6605554)
})

## Test that plotting works?
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("length", "width"),
                        multi_plot = FALSE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("length", "height"),
                        multi_plot = FALSE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("width", "length"),
                        multi_plot = FALSE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("width", "height"),
                        multi_plot = FALSE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("height", "length"),
                        multi_plot = FALSE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("height", "width"),
                        multi_plot = FALSE)
dev.off()

pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("length", "width"),
                        multi_plot = TRUE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("length", "height"),
                        multi_plot = TRUE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("width", "length"),
                        multi_plot = TRUE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("width", "height"),
                        multi_plot = TRUE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("height", "length"),
                        multi_plot = TRUE)
dev.off()
pdf(file = NULL)
plot_viewr_trajectories(motive_full,
                        plot_axes = c("height", "width"),
                        multi_plot = TRUE)
dev.off()

## Test arguments to plot_viewr_trajectories()
test_that("plot_viewr_trajectories() fails when nonsense is supplied",{
  expect_error(plot_viewr_trajectories(motive_full,
                                       plot_axes = c("length", "width"),
                                       multi_plot = jobin))
  # expect_error(plot_viewr_trajectories(motive_full,
  #                                      plot_axes = c("length", "height"),
  #                                      multi_plot = jobin))
  # expect_error(plot_viewr_trajectories(motive_full,
  #                                      plot_axes = c("width", "length"),
  #                                      multi_plot = jobin))
  # expect_error(plot_viewr_trajectories(motive_full,
  #                                      plot_axes = c("width", "height"),
  #                                      multi_plot = jobin))
  # expect_error(plot_viewr_trajectories(motive_full,
  #                                      plot_axes = c("height", "length"),
  #                                      multi_plot = jobin))
  # expect_error(plot_viewr_trajectories(motive_full,
  #                                      plot_axes = c("height", "width"),
  #                                      multi_plot = jobin))
})

# # test plot output w/vdiffr
# multi_plotdefault <-
#   plot_viewr_trajectories(motive_full,
#                           plot_axes = c("length", "width"),
#                           multi_plot = TRUE)
# multi_plotlh <-
#   plot_viewr_trajectories(motive_full,
#                           plot_axes = c("length", "height"),
#                           multi_plot = TRUE)
# multi_plotwl <-
#   plot_viewr_trajectories(motive_full,
#                           plot_axes = c("width", "length"),
#                           multi_plot = TRUE)
# multi_plotwh <-
#   plot_viewr_trajectories(motive_full,
#                           plot_axes = c("width", "height"),
#                           multi_plot = TRUE)
# multi_plothl <-
#   plot_viewr_trajectories(motive_full,
#                           plot_axes = c("height", "length"),
#                           multi_plot = TRUE)
# multi_plothw <-
#   plot_viewr_trajectories(motive_full,
#                           plot_axes = c("height", "width"),
#                           multi_plot = TRUE)
# #use addins to open shiny app to validate plots
# test_that("plot_viewr_trajectories() plot output is OK", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#   skip_on_gh_actions
#   vdiffr::expect_doppelganger(
#     "multi plot default",
#     plot_viewr_trajectories(
#       motive_full,
#       plot_axes = c("length", "width"),
#       multi_plot = TRUE
#     )
#   )
#   vdiffr::expect_doppelganger(
#     "multi plot lh",
#     plot_viewr_trajectories(
#       motive_full,
#       plot_axes = c("length", "height"),
#       multi_plot = TRUE
#     )
#   )
#   vdiffr::expect_doppelganger(
#     "multi plot wl",
#     plot_viewr_trajectories(
#       motive_full,
#       plot_axes = c("width", "length"),
#       multi_plot = TRUE
#     )
#   )
#   vdiffr::expect_doppelganger(
#     "multi plot wh",
#     plot_viewr_trajectories(
#       motive_full,
#       plot_axes = c("width", "height"),
#       multi_plot = TRUE
#     )
#   )
#   vdiffr::expect_doppelganger(
#     "multi plot hl",
#     plot_viewr_trajectories(
#       motive_full,
#       plot_axes = c("height", "length"),
#       multi_plot = TRUE
#     )
#   )
#   vdiffr::expect_doppelganger(
#     "multi plot hw",
#     plot_viewr_trajectories(
#       motive_full,
#       plot_axes = c("height", "width"),
#       multi_plot = TRUE
#     )
#   )
# })
