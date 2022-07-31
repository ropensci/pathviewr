# Tests of visualize_frame_gap_choice()

## test of object input
test_that(
  "visualize_frame_gap_choice() fails when data are missing or nonsense args",
          {
            expect_error(visualize_frame_gap_choice(obj_name = "steve"))
            expect_error(visualize_frame_gap_choice(loops = TRUE))
            #expect_error(visualize_frame_gap_choice(loops = "steve"))
            expect_error(visualize_frame_gap_choice())
          })

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                              package = 'pathviewr'))

motive_selected <-
  motive_data %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  get_velocity() %>%
  select_x_percent(desired_percent = 50)

# make a bunch of empty vectors to dump info
mfg <- vector("list", 10)
cts <- vector("list", 10)
trajectory_count <- vector(mode = "double", 10)
frame_gap_allowed <- vector(mode = "double", 10)

#test empty vectors
test_that("vectors are empty before looping", {
  expect_equal(mfg[[3]], NULL)
  expect_equal(cts[[10]], NULL)
  expect_equal(trajectory_count[[6]], 0)
  expect_equal(frame_gap_allowed[[4]], 0)
})

#test tibble output
test_that("visualize_frame_gap_choice() tibble output is OK", {
  expect_equal(
    visualize_frame_gap_choice(motive_selected,
                               loops = 10)[[1]]$trajectory_count[[5]],
               13)
  expect_equal(
    visualize_frame_gap_choice(motive_selected,
                               loops = 10)[[1]]$frame_gap_allowed[[8]],
               8)
})

#test that plotting works?
pdf(file = NULL)
visualize_frame_gap_choice(motive_selected,
                           loops = 20)[[2]]
dev.off()

## Test arguments to plot_by_subject()
test_that("visualize_frame_gap_choice() fails when nonsense is supplied",
          {
            expect_error(visualize_frame_gap_choice(motive_selected,
                                                    loops = jobin))
          })

#test plot output w/vdiffr

# #use addins to open shiny app to validate plots
#  test_that("visualize_frame_gap_choice() plot output is OK", {
#    skip_on_cran()
#    skip_on_travis()
#    skip_on_appveyor()
#    skip_on_gh_actions
#    vdiffr::expect_doppelganger("mfg plot",
#                                visualize_frame_gap_choice(motive_selected,
#                                                           loops = 10)[[2]])
#  })
