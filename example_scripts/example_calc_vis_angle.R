## Last updated 2020/05/28 by ERP

#####     Worked example for using calc_vis_angle functions      #####


## The calc_vis_angle functions reference the Position_widths and Position_heights
## variables of raw motive data files and require the following arguments:

##  Ground plane (gnd_plane) - at baseline, this indicates the vertical distance
##    between the bottom of the "V" and the height of the grounding tool.
##    Following raw data processing using the utility functions, it must indicate
##    the vertical distance between the bottom of the "V" and the height of the
##    perches. An incorrect gnd_plane for a given tunnel set up may calculate
##    negative visual angles (as if the bird is flying outside the tunnel).
##    gnd_plane should be reported in the same units as Position_widths and
##    Position_heights (meters I believe).
##  Vertex angle (vertex_angle) - this is included in the calc_vis_angle_mod
##    functions and it represents the acute angle each screen creates with a
##    vertical axis. It is also equal to the angle of the "V" divided by 2.
##    It's reported in degrees (because who measures things in radians anyway)
##    and is converted to radians within the functions so that it works with the
##    trig functions.
##  Stimulus parameter (stim_param) - this reflects the size of the visual
##    stimulus being displayed on either side of the tunnel. For example, if the
##    screens display 10cm wide vertical or horizontal bars, stim_param = 0.1.

###   NOTE: Perhaps these arguments can eventually be supplied or referenced
###   from within each file's metadata.


## The first several sections of this example simply run through the steps of
## importing and cleaning raw data files using the utility functions written by
## VBB.
## The calc_vis_angle functions will be run on the these objects after they've
## been processed


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

# Lets try an examples using the all in one function (import_and_clean_motive)

                   #####   All in one function   #####
jul_29_path <- './inst/extdata/july-29_group-I_16-20.csv'


jul_29_all_defaults <-
  jul_29_path %>% import_and_clean_motiv()


## or we could use the utility functions in sequence

                #####   Utility functions in sequence   #####
## default arguments used for every step to see if the product mirrors the all
## in one function output

          #####   Data import   #####
jul_29 <- read_motiv_csv('./inst//extdata/july-29_group-I_16-20.csv')

          #####   Rename axes   #####
jul_29 <- relabel_viewr_axes(jul_29,
                             tunnel_length = "_Z",
                             tunnel_width = "_X",
                             tunnel_height = "_Y")


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
## beautiful



        ################    calc_vis_angle functions      #############

              ####    calc_vis_angle_1 and calc_vis_angle_1.1   ####
## Assuming fixed horizontal gaze, calculates the visual angle subtended by the
## length of a stimulus feature on either screen and the eye of the bird.
## Positive and negative sides of tunnel reflect positive and negative
## values of "Position_widths".
# # # # #  NOTE THIS FUNCTION ONLY WORKS WITH VERTEX ANGLE = 45˚  # # # # # #

jul_29_all_defaults_1 <-
calc_vis_angle_1(jul_29_all_defaults,
                 gnd_plane = 0.5,
                 stim_param_pos = 0.1,
                 stim_param_neg = 0.1)

jul_29_full_1 <-
  calc_vis_angle_1(jul_29_full,
                   gnd_plan = 0.5,
                   stim_param_pos = 0.1,
                   stim_param_neg = 0.1)
# produces new variables for all calculations internal to the function

## check to see if everthing worked out the same
identical(jul_29_all_defaults_1, jul_29_full_1)
## noice.

View(jul_29_full_1)


# This version produces new variables only for the visual angles perceived on the
# positive and negative sides of the tunnel.
jul_29_all_defaults_1.1 <-
  calc_vis_angle_1.1(jul_29_all_defaults,
                   gnd_plane = 0.5,
                   stim_param_pos = 0.1,
                   stim_param_neg = 0.1)

jul_29_full_1.1 <-
  calc_vis_angle_1.1(jul_29_full,
                     gnd_plane = 0.5,
                     stim_param_pos = 0.1,
                     stim_param_neg = 0.1)


## check to see if everthing worked out the same
identical(jul_29_all_defaults_1.1, jul_29_full_1.1)
## double noice.

View(jul_29_full_1.1)



              ####    calc_vis_angle_2 and calc_vis_angle_2.1   ####
## Assuming gaze is fixed at the point on each screen such that the axis of gaze
## is orthogonal to the plane of each screen. This function minimizes the distance
## to both screens and therefore maximizes the visual angles calculated.
# # # # #  NOTE THIS FUNCTION ONLY WORKS WITH VERTEX ANGLE = 45˚  # # # # # #


jul_29_all_defaults_2 <-
  calc_vis_angle_2(jul_29_all_defaults,
                   gnd_plane = 0.5,
                   stim_param_pos = 0.1,
                   stim_param_neg = 0.1)

jul_29_full_2 <-
  calc_vis_angle_2(jul_29_full,
                   gnd_plane = 0.5,
                   stim_param_pos = 0.1,
                   stim_param_neg = 0.1)
# produces new variables for all calculations internal to the function

## check to see if everthing worked out the same
identical(jul_29_all_defaults_2, jul_29_full_2)
## shablam.

View(jul_29_full_2)


# This version produces new variables only for the visual angles perceived on the
# positive and negative sides of the tunnel.
jul_29_all_defaults_2.1 <-
  calc_vis_angle_2.1(jul_29_all_defaults,
                   gnd_plane = 0.5,
                   stim_param_pos = 0.1,
                   stim_param_neg = 0.1)

jul_29_full_2.1 <-
  calc_vis_angle_2.1(jul_29_full,
                     gnd_plane = 0.5,
                     stim_param_pos = 0.1,
                     stim_param_neg = 0.1)

## check to see if everthing worked out the same
identical(jul_29_all_defaults_2, jul_29_full_2)
## double shablam.

View(jul_29_full_2.1)



          ####    calc_vis_angle_mod and calc_vis_angle_mod.1   ####
## Assuming gaze is fixed at the point on each screen such that the axis of gaze
## is orthogonal to the plane of each screen, this function function accomodates
## for different vertex angles that the tunnel may be set at. It minimizes the
## distance to the screen closer to the bird and therefore maximizes the visual
## angle calculated for the closer screen.
## However, this does not hold true for the opposite screen in the following
## condition. If the vertex angle of the tunnel is > 45˚ and the bird's position
## extends beyond the boundary created by a 45˚ vertex angle, i.e. the bird is
## quite close to one screen, then the closest point to the opposite screen is the
## vertex itself. Given that the birds tend to fly fairly close to the center of
## the tunnel, this may not be encountered very often.

jul_29_all_defaults_mod <-
  calc_vis_angle_mod(jul_29_all_defaults,
                     gnd_plane = 0.5,
                     vertex_angle = 45,
                     stim_param_pos = 0.1,
                     stim_param_neg = 0.1)

jul_29_full_mod <-
  calc_vis_angle_mod(jul_29_full,
                     gnd_plane = 0.5,
                     vertex_angle = 45,
                     stim_param_pos = 0.1,
                     stim_param_neg = 0.1)
# produces new variables for all calculations internal to the function

## check to see if everthing worked out the same
identical(jul_29_all_defaults_mod, jul_29_full_mod)
## bazinga!

View(jul_29_full_mod)


# This version produces new variables only for the visual angles perceived on the
# positive and negative sides of the tunnel.
jul_29_all_defaults_mod.1 <-
  calc_vis_angle_mod.1(jul_29_all_defaults,
                     gnd_plane = 0.5,
                     vertex_angle = 45,
                     stim_param_pos = 0.1,
                     stim_param_neg = 0.1)

jul_29_full_mod.1 <-
  calc_vis_angle_mod.1(jul_29_full,
                       gnd_plane = 0.5,
                       vertex_angle = 45,
                       stim_param_pos = 0.1,
                       stim_param_neg = 0.1)

## check to see if everthing worked out the same
identical(jul_29_all_defaults_mod.1, jul_29_full_mod.1)
## you get it now

View(jul_29_full_mod.1)



## because vertex angle is set to 45˚, calc_vis_angle_2 and calc_vis_angle_mod
## should produce identical results
identical(jul_29_full_2, jul_29_full_mod)
identical(jul_29_full_2.1, jul_29_full_mod.1)
# WAT!?
# it looks the same. I don't get it...


          ##############    Next Steps    #############
## Assuming I figure out what's going on  above, the next steps would be to:
##
##  1. Improve the latest function (calc_vis_angle_mod) by including head
##  orientation to calculate visual angles based on where the bird
##  is actually looking. This could take a while
##
##  2. Figure out how to visualize or otherwise use the visual angles calculated
##  from these functions.
##    - I believe this variable can be the nucleus with which to design functions
##    that more accurately estimate optic flow by calculating how it changes over
##    time in various dimensions, i.e. how it increases or decreases with the
##    bird's movements.


##      Thoughts?




















