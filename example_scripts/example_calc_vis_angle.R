## Last updated 2020/07/16 by ERP

##########################  Not fully worked out yet


## The calc_vis_angle function references the position_width and position_height
## variables of raw motive data files and require the following arguments:

##  gnd_plane - at baseline, this indicates the vertical distance
##    between the bottom of the "V" and the height of the grounding tool.
##    Following raw data processing using the utility functions, it must
##    indicate the vertical distance between the bottom of the "V" and the
##    height of the perches. An incorrect gnd_plane for a given tunnel set up
##    may calculate negative visual angles (as if the bird is flying outside the
##    tunnel). gnd_plane should be reported in the same units as position_width
##    and position_height (typically in meters).
##
##  stim_param_pos AND stim_param_neg - these reflects the
##    size of the visual stimulus displayed on either side of the tunnel.
##    E.g. positive screens displays 10cm wide bars: stim_param_pos = 0.1.
##
##  vertex_angle - this is included in the calc_vis_angle_mod
##    functions and it represents the acute angle each screen creates with a
##    vertical axis. It is also equal to the angle of the "V" divided by 2.
##    It's reported in degrees and is converted to radians so that it works with
##    the trig functions.
##
##  simplify_output - if set to TRUE, this limits output variables to
##  vis_angle_pos_deg and vis_angle_neg_deg

###   NOTE: Perhaps these arguments can eventually be supplied or referenced
###   from within each file's metadata.
###   7.14.2020 - perhaps gnd_plane, stim_param_pos, stim_param_neg, and
###   vertx_angle instead these can be referenced from the data frame itself
###   after the user has used insert_treatments.


## The first several sections of this example simply run through the importing
## and cleaning pipeline to generate an object that calc_vis_angle can apply to.

                      #####   Load packages   #####
library(tidyverse)


                     #####   Source scripts    #####
## First define the directory to inspect
scripts_path <- "./R/"
## calc_vis_angle functions now included in this path

## Now list all the .R files
scripts_list <- list.files(path = scripts_path,
                           pattern = "*.R",
                           full.names = TRUE,
                           recursive = TRUE
)

## Using a for() loop instead of lapply() because the latter produces messages
## we don't need.
for (i in 1:length(scripts_list)) {
  source(scripts_list[i])
}


             #####     Working with motive objects      #####

# Lets try an examples using the all in one function (import_and_clean_viewr)

                   #####   All in one function   #####
jul_29_path <- './inst/extdata/july-29_group-I_16-20.csv'


jul_29_all_defaults <-
  jul_29_path %>% import_and_clean_viewr()


## or we could use the utility functions in sequence

                #####   Utility functions in sequence   #####
## default arguments used for every step to see if the product mirrors the all
## in one function output

          #####   Data import   #####
jul_29 <- read_motive_csv('./inst//extdata/july-29_group-I_16-20.csv')

          #####   Rename axes   #####
jul_29 <- relabel_viewr_axes(jul_29,
                             tunnel_length = "_z",
                             tunnel_width = "_x",
                             tunnel_height = "_y",
                             real = "_w")


          #####    Gather data   #####
jul_29_gathered <- gather_tunnel_data(jul_29)

          #####   Trim outliers   #####
jul_29_trimmed <- trim_tunnel_outliers(jul_29_gathered)

          #####   Rotate tunnel   #####
jul_29_rotated <-
  jul_29_trimmed %>% rotate_tunnel()

        #####   Select X percent    #####
jul_29_selected <-
  jul_29_rotated %>% select_x_percent()

      #####   Separate trajectories    #####
jul_29_labeled <-
  jul_29_selected %>% separate_trajectories()

      #####   Keep full trajectories    #####
jul_29_full <-
  jul_29_labeled %>% get_full_trajectories()

identical(jul_29_all_defaults, jul_29_full)
## curently false as import_and_clean_viewr includes velocity calculations



        ################    calc_vis_angle      #############

## For an experiment with the ground plane set 50cm above the bottom of a 45˚
## "V" and sine-wave gratings with a period of 10cm displayed on either side of
## the tunnel, the following arguments would be used.
## NOTE: in this case, the position of the grounding tool was unknown and
## therefore gnd_plane is estimated as the height of the perches from the base
## of the "V".

full45 <- calc_vis_angle(jul_29_all_defaults,
                    gnd_plane = 0.5,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.1,
                    vertex_angle = 45,
                    simplify_output = FALSE)
View(full45)
## this produces 10 additional variables in the dataframe based on the math
## internal to the function.

## use simplify_output = TRUE to output only the degrees of visual angle on the
## positive and negative sides of the tunnel

simp45 <- calc_vis_angle(jul_29_all_defaults,
                    gnd_plane = 0.5,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.1,
                    vertex_angle = 45,
                    simplify_output = TRUE)
View(simp45)

## One major limitations arises from experiments where vertex_angle > 45˚.

## If the exact same data was obtained in a tunnel set even slightly differently
## at vertex_angle = 47˚, the output produces negative visual angles (nonsense)
full60 <- calc_vis_angle(jul_29_all_defaults,
                    gnd_plane = 0.5,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.1,
                    vertex_angle = 60,
                    simplify_output = FALSE)

## This is becuase calculating width_2_screen inside the function works properly
## up to a virtual boundary that extends at a right angle from either screen.
## This means calc_vis_angle is likely to produce errors in tunnel arrangements
## where vertex_angle > 45˚.


View(test1)
View(test3)



          ##############    Next Steps    #############

##  1. Set gnd_plane automatically by using the distance from the bottom of the
##  "V" to (0,0,0) by referencing something within rotate_tunnel().
##
##  2. Set vertex_angle and stim_param arguments from an insert_treatments()
##  function elsewhere in the pipeline.
##
##  ** external to calc_vis_angle - include ordientation coordinates in
##  simple plots such that each point represents position and head orientation
##
##  3. Include head orientation to calculate visual angles based on where the
##  bird is actually looking rather than assuming a fixed gaze.
##  This could take a while...
##
##  4. Figure out how to visualize or otherwise use the visual angles calculated
##  from these functions.
##    - I believe this variable can be the nucleus with which to design functions
##    that more accurately estimate optic flow by calculating how it changes over
##    time in various dimensions, i.e. how it increases or decreases with the
##    bird's movements.






















