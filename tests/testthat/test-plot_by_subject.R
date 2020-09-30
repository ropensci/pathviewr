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
test_that("plot_by_subject() default plot output is OK", {
  vdiffr::expect_doppelganger("plot by subject colbytreat 1",
                              plot_by_subject(motive_full, col_by_treat = TRUE)[[1]])
  vdiffr::expect_doppelganger("plot by subject colbytreat 2",
                              plot_by_subject(motive_full, col_by_treat = TRUE)[[2]])
})
