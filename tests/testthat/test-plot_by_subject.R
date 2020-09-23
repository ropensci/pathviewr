# # Tests of plot_by_subject()
#
# ## Import the example Motive data included in the package
# motive_data <-
#   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#                               package = 'pathviewR'))
#
# ## Clean, isolate, and label trajectories
# motive_full <-
#   motive_data %>%
#   clean_viewr(desired_percent = 50,
#               max_frame_gap = "autodetect",
#               span = 0.95)
#
# ## Plot all trajectories by subject
# motive_full %>%
#   plot_by_subject()
#
# ## Add treatment information
# motive_full$treatment <- c(rep("latA", 100), rep("latB", 100),
#                            rep("latA", 100), rep("latB", 149))
#
# ## Plot all trajectories by subject, color by treatment
# motive_full %>%
#   plot_by_subject(col_by_treat = TRUE)
