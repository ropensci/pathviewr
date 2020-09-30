#Tests of plot_by_subject()
context("plot by subject")
library(tidyverse)

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Clean, isolate, and label trajectories
motive_full <-
  motive_data %>%
  clean_viewr(desired_percent = 50,
              max_frame_gap = "autodetect",
              span = 0.95)

#test axes limits
width_limits <- c(max(abs(range(
  motive_full$position_width
))) * -1,
max(abs(range(
  motive_full$position_width
))))

test_that("base functions set axes correctly", {
  expect_equal(max(abs(range(
    motive_full$position_height
  ))) * -1, -0.243158)
  expect_equal(max(abs(range(
    motive_full$position_width
  ))), width_limits[[2]])
})

# if (col_by_treat == FALSE)
#for top view
top_view <- motive_full %>%
  dplyr::group_by(subject) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    paths = purrr::map(
      data,
      ~ ggplot2::ggplot(., aes(
        position_length,
        position_width
      )) +
        geom_point(alpha = .1, show.legend = FALSE) +
        ylim(width_limits) +
        geom_hline(yintercept = 0, linetype = "dotted") +
        coord_fixed(ratio = 1)
    ),
    hist = purrr::map(
      data,
      ~ ggplot2::ggplot(., aes(x = position_width)) +
        geom_density(
          alpha = .5,
          position = "identity",
          show.legend = FALSE
        ) +
        xlim(width_limits) +
        geom_vline(xintercept = 0, linetype = "dotted") +
        coord_flip()
    )
  )
#all of them together:
top_all_plots <- top_view %>%
  dplyr::select(subject, paths, hist) %>%
  tidyr::gather("plot_type", "allplots", 2:3)

#test top views
test_that("top views created correctly via purrr::map", {
  expect_equal(top_view[[3]][[1]][["data"]][["position_width"]][[2]],-.1163564)
  expect_equal(environment(top_view[[4]][[3]][["facet"]][["super"]])[["args"]], NULL)
})

test_that("top views wrangled correctly via tidyverse", {
  expect_match(top_all_plots$plot_type[[1]], "paths")
  expect_match(top_all_plots$subject[[5]], "device03")
  expect_match(top_all_plots[[3]][[4]][["labels"]][["x"]], "position_width")
})

#test plot output w/vdiffr
#use addins to open shiny app to validate plots
test_that("plot_by_subject() default plot output is OK", {
  vdiffr::expect_doppelganger("plot by subject default 1",
                              plot_by_subject(motive_full)[[1]])
  vdiffr::expect_doppelganger("plot by subject default 2",
                              plot_by_subject(motive_full)[[2]])
})

## Add treatment information
motive_full$treatment <- c(rep("latA", 100), rep("latB", 100),
                           rep("latA", 100), rep("latB", 149))

## Plot all trajectories by subject, color by treatment
motive_full %>%
  plot_by_subject(col_by_treat = TRUE)

#test plot output w/vdiffr
#use addins to open shiny app to validate plots
test_that("plot_by_subject() col_by_treat plot output is OK", {
  vdiffr::expect_doppelganger("plot by subject colbytreat 1",
                              plot_by_subject(motive_full, col_by_treat = TRUE)[[1]])
  vdiffr::expect_doppelganger("plot by subject colbytreat 2",
                              plot_by_subject(motive_full, col_by_treat = TRUE)[[2]])
})
