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

## Source un-exported pathviewr things too
devtools::load_all()

########################### example roz2016 object #############################

## Test pancake
test_mat <-
  read_flydra_mat(
    "./inst/extdata/pathviewr_flydra_example_data.mat",
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



### estimate_sf_box

calc_sf_box <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name, "pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments has been run
  if (!any(attr(obj_name, "pathviewr_steps") == "treatments_added")){
    stop("Please run insert_treatments() prior to use")
  }

  ## Calculate minimum distance to each screen
  obj_name$min_dist_pos <- abs(obj_name$pos_wall - obj_name$position_width)
  obj_name$min_dist_neg <- abs(obj_name$neg_wall - obj_name$position_width)

  ## Calculate distance along plane of the wall equal to 1˚ of visual angle.
  deg_dist_pos <- 2 * obj_name$min_dist_pos * tan(deg_2_rad(1))
  deg_dist_neg <- 2 * obj_name$min_dist_neg * tan(deg_2_rad(1))

  ## Calculate spatial frequency as number of cycles of stimulus per 1˚ of
  ## visual angle.
  obj_name$s_freq_pos <- deg_dist_pos / obj_name$stim_param_pos
  obj_name$s_freq_neg <- deg_dist_neg / obj_name$stim_param_neg

  ## Leave note that spatial frequencies were calculated on dataset
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                         "frequencies_calculated")

  return(obj_name)
}


#### testing zone ####

roz <- insert_treatments(test_cleaned,
                         pos_wall = 0.5,
                         neg_wall = -0.5,
                         front_wall = 1.5,
                         treatment = "latA")


roz <- calc_sf_box(roz)

View(roz)

ggplot(roz, aes(position_width, position_height)) +
  geom_point(size = 4, aes(color = s_freq_pos))

ggplot(roz, aes(position_width, position_height)) +
  geom_point(size = 4, aes(color = s_freq_neg))



