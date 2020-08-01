
#### load shit ####
packages <- c("devtools",
              "tidyverse",
              "readxl",
              "R.matlab",
              "ggthemes",
              "gridExtra",
              "data.table",
              "rgl")
package.check <- lapply(packages, # applies the function to a list and returns
                        # a list the same length
                        FUN = function(x) {
                          if (!require(x, character.only = TRUE)) { # same as
                            # library but for use inside functions
                            install.packages(x, dependencies = TRUE)
                            library(x, character.only = TRUE)
                          }
                        }
)

## pathviewr
library(pathviewR)

## Source un-exported pathviewR things too
devtools::load_all()

## Import and clean data
jul_29_mfg_autodetect <-
  jul_29_path %>% import_and_clean_viewr(max_frame_gap = "autodetect",
                                         frame_gap_messaging = TRUE)

## plot all trajectories
plot_viewr_trajectories(jul_29_mfg_autodetect, multi_plot = TRUE)
## trajectory #63 has a nice big gap

## get a list of trajectories
trajs <- unique(jul_29_mfg_autodetect$file_sub_traj)

## isolate a gappy trajectory (#63)
gap_dat <-
  jul_29_mfg_autodetect %>%
  filter(file_sub_traj == trajs[63])
## Confirm it's the right one
plot(gap_dat$position_length,
     gap_dat$position_width,
     asp = 1)


##### interpolate data via loess #####
## Instead of interpolating 3D trajectory data, it would be
## wiser to interpolate within a function of frame ~ axis for
## each of the three axes.

library(fANCOVA)
length_fit <- fANCOVA::loess.as(gap_dat$frame,
                                gap_dat$position_length,
                                plot = TRUE)
frame_range <- seq(from = min(gap_dat$frame),
                  to = max(gap_dat$frame),
                  by = 1)
length_preds <- predict(length_fit, frame_range)
plot(gap_dat$frame,
     gap_dat$position_length)
plot(frame_range,
     length_preds)

## Do the same for width and height
width_fit <- fANCOVA::loess.as(gap_dat$frame,
                               gap_dat$position_width,
                               plot = TRUE)
width_preds <- predict(width_fit, frame_range)
height_fit <- fANCOVA::loess.as(gap_dat$frame,
                                gap_dat$position_height,
                                plot = TRUE)
height_preds <- predict(height_fit, frame_range)

## put it all together
predicted_data <- tibble(frame = frame_range,
                         position_length = length_preds,
                         position_width = width_preds,
                         position_height = height_preds)

## Original data plot
plot(gap_dat$position_length,
     gap_dat$position_width,
     asp = 1)
## predicted data plot
## Original data plot
plot(predicted_data$position_length,
     predicted_data$position_width,
     asp = 1)
