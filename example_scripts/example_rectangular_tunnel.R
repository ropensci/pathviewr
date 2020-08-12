## Thanks Eric!!!

############################### package loading ################################
## Specify the packages you'll use in the script
packages <- c("devtools",
              "tidyverse",
              "readxl",
              "R.matlab",
              "ggthemes",
              "gridExtra",
              "data.table",
              "rgl")
## Now for each package listed, first check to see if the package is already
## installed. If it is installed, it's simply loaded. If not, it's downloaded
## from CRAN and then installed and loaded.
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

## Source un-exported pathviewR things too
devtools::load_all()

########################### example roz2016 object #############################

## Test pancake
test_mat <-
  read_flydra_mat(
    "./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5-short-only.mat",
    subject_name = "steve")

## Clean it up -- these are the settings I've been using
## No need for relabeling or gathering -- Flydra handles that.
test_cleaned <-
  test_mat %>%
  redefine_tunnel_center(length_method = "middle",
                         width_method = "original",
                         height_method = "original") %>%
  get_velocity(add_to_viewr = TRUE) %>%
  select_x_percent(desired_percent = 80) %>%
  separate_trajectories(max_frame_gap = 1) %>%
  get_full_trajectories(span = 0.95)

## Quick plot:
plot(test_cleaned$position_length,
     test_cleaned$position_width,
     asp = 1)
## Some notes about the tunnel:
## Width is centered at 0. I believe the tunnel extends from +0.5 to -0.5 on
## the width axis but I will need to double-check
## I am also unsure how the stimulus parameters look -- the stimuli themselves
## vary a lot among the different experiments. It would be great to discuss
## with you how the spatial frequency parameter could be used as an argument. I
## am happy if you keep it in the same format as your other functions for now,
## i.e. with a stim_param_pos and stim_param_neg
