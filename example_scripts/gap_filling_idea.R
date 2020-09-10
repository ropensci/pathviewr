#### load stuff ####
packages <-
  c(
    "devtools",
    "tidyverse",
    "readxl",
    "R.matlab",
    "ggthemes",
    "gridExtra",
    "data.table",
    "rgl"
  )
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)}})

## Source un-exported pathviewR things too
devtools::load_all()

############################## quick data import ###############################
motive_data_path <- './inst/extdata/pathviewR_motive_example_data.csv'

motive_data_mfg_autodetect <-
  motive_data_path %>% import_and_clean_viewr(max_frame_gap = "autodetect",
                                              frame_gap_messaging = TRUE)

## plot all trajectories
plot_viewr_trajectories(motive_data_mfg_autodetect, multi_plot = TRUE)
## trajectory #12 has a nice big gap

## get a list of trajectories
trajs <- unique(motive_data_mfg_autodetect$file_sub_traj)

## isolate a gappy trajectory (#12)
gap_dat <-
  motive_data_mfg_autodetect %>%
  filter(file_sub_traj == trajs[12])
## Confirm it's the right one
plot(gap_dat$position_length,
     gap_dat$position_width,
     asp = 1)

## Set up for functionization
motive_data_mfg_autodetect -> obj_name

#### gap filling function #####

## PORTED OVER TO UTILITY FUNCTIONS!

#### test on same file ####
## plot all trajectories (before)
plot_viewr_trajectories(motive_data_mfg_autodetect, multi_plot = TRUE)

motive_filling <-
  motive_data_mfg_autodetect %>%
  fill_traj_gaps()

## plot all trajectories again
plot_viewr_trajectories(motive_filling, multi_plot = TRUE)


#### test on another file ####
test_data_path <- './inst/extdata/july-29_group-I_16-20.csv'

test_data_mfg_autodetect <-
  test_data_path %>% import_and_clean_viewr(max_frame_gap = "autodetect",
                                              frame_gap_messaging = TRUE)

## plot all trajectories
plot_viewr_trajectories(test_data_mfg_autodetect, multi_plot = TRUE)

test_filling <-
  test_data_mfg_autodetect %>%
  fill_traj_gaps()

## plot all trajectories again
plot_viewr_trajectories(test_filling, multi_plot = TRUE)
