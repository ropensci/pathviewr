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
library(tidyverse) # used only for plotting at the end

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
## This produces 10 additional variables in the dataframe based on the math
## internal to calc_vis_angle.

    #### _VBB_questions_1 #####
    ## In the calculation:
    ##   obj_name$height_2_screen <- obj_name$height_2_vertex -
    ## (abs(obj_name$position_width) / tan(vertex_angle))
    ##
    ## Should it not be:
    ##   obj_name$height_2_screen <- obj_name$height_2_vertex -
    ## (abs(obj_name$position_width) / tan(vertex_angle/2))
    ##
    ## Just to check -- the way stim_param_pos and _neg work imply that
    ## the stimulus is a horizontal pattern, right? To put it another
    ## way, I don't see how a vertically-oriented pattern can be used
    ## with the dimensions along which visual angles are being recorded.
    ##
    ## Both of thes really could just be a lack of understanding on my
    ## part, so it would be great if we could zoom about this so I can
    ## get a better sense. I will send you another doc with illustrations
    ## to see if we can hash this out.


## Use simplify_output = TRUE to output only the degrees of visual angle on the
## positive and negative sides of the tunnel
simp45 <- calc_vis_angle(jul_29_all_defaults,
                    gnd_plane = 0.5,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.1,
                    vertex_angle = 45,
                    simplify_output = TRUE)
View(simp45)

    #### _VBB_suggestions_1 #####
    ## Just a small suggestion -- could min_dist (pos and neg) be valuable
    ## to include in the simplifed output too? I think distance to the
    ## walls could come in handy.

## calc_vis_angle can accomodate different tunnel vertex angles and now
## calculates mind_dist correctly even when the bird is flying far from the
## center line.
## NOTE: This data was collected at vertex_angle = 45 so changing this argument
## is not something a user would want to do. But let's see how it affects the
## values and distribution of angles in the tunnel.
full60 <- calc_vis_angle(jul_29_all_defaults,
                    gnd_plane = 0.5,
                    stim_param_pos = 0.1,
                    stim_param_neg = 0.1,
                    vertex_angle = 60,
                    simplify_output = FALSE)
View(full60)


###### Visualizing the calculated angles by position in the tunnel  #####

## Plot position in cross section of the tunnel and color points by
## vis_angle_pos_deg
ggplot(full45, aes(x = position_width, y = position_height)) +
  geom_point(aes(color = vis_angle_pos_deg),
             shape = 1, size = 3)
# greater visual angles closer to positive (right) side of the tunnel

## Now for vertex_angle = 60
ggplot(full60, aes(x = position_width, y = position_height)) +
  geom_point(aes(color = vis_angle_pos_deg),
             shape = 1, size = 3)
# flipping between the graphs you can see a subtle shift in the distribution of
# angles.

    #### _VBB_suggestions_2 #####
    ## These plots are awesome! Easy to make sense of them.
    ##
    ## One feature that would also be helpful would be to add a visualization of
    ## the tunnel itself, which would require inferring the coordinates of the
    ## tunnel. I think we can reasonably approximate some of this using the
    ## inputs from calc_vis_angle() alone.

    ## Here's what we assume:
    ## - With gnd_plane = 0.5, we assume the vertex is at -0.5 on the height
    ##   axis. Therefore the vertex is located at (0, -0.5) for (width, height);
    ##   ignoring length for now.
    ## - With vertex_angle = 45, we assume the vertex angle (duh).
    ## - It may not be feasible to know the height of the tunnel, i.e. where
    ##   on the position_height axis the walls actually end. So, I suggest we
    ##   use position_height = 0 as a first-pass approximation. Therefore, the
    ##   two topmost points of the tunnel walls will be at (?, 0) and (-?, 0),
    ##   for the positive and negative side, respectively (again, ignore length)

    ## So, with those conditions, it's time for some SohCahToa:
    ##   For the positive side:
    ##     tan(deg2rad(45/2)) = x / 0.5
    ##     0.5 * tan(deg2rad(45/2)) = x
    ##     0.2071068 = x
    ##   Therefore, the topmost point on the positive side is at (0.207, 0)
    ##   Since the tunnel is assumed symmetric about width = 0, the topmost
    ##   point on the neg side is (-0.207,0).
    ##   And for fun, the length of the hypotenuse, h, (i.e. the length of the
    ##   walls themselves) is:
    ##     cos(deg2rad(45/2)) = 0.5 / h
    ##     h = 0.5 / cos(deg2rad(45/2))
    ##     h = 0.5411961
    ##   As a check:
       sqrt((0.2071068 ^2) + (0.5^2)) ## equates to 0.5411961

    ## Also if we use the three points (two topmost and vertex), we should
    ## be able to back-calculate the vertex angle to confirm it is ~45
       xyangles(x1 = -0.2071, ## (x1, y1) is topmost on negative side
                y1 = 0,
                x2 = 0,       ## (x2, y2) is vertex
                y2 = -0.5,
                x3 = 0.2071,  ## (x3, y3) is topmost on positive side
                y3 = 0)
       ## I get 44.99867, which is basically 45. So this all seems to work.

    ## Yoinking the vis_angle_pos_deg plot and adding walls:
       ggplot(full45, aes(x = position_width, y = position_height)) +
         geom_point(aes(color = vis_angle_pos_deg),
                    shape = 1, size = 3) +
         geom_segment(aes(x = 0,         ## POSITIVE SIDE FIRST
                          y = -0.5,
                          xend = 0.207,
                          yend = 0)) +
         geom_segment(aes(x = 0,         ## NEGATIVE SIDE
                          y = -0.5,
                          xend = -0.207,
                          yend = 0))

    ## Judging by this visualization, I think either the vertex angle is
    ## actually greater than 45 (i.e. the "V" is wider than 45deg) or the
    ## ground plane placement should be lower.
    ## Is it worth re-measuring the tunnel in lab?

    ## Related to all this, the information of tunnel coordinates can be
    ## inferred from user inputs to cal_vis_angle. I think it would be
    ## nice to add these coordinates in as attributes to the object. That
    ## way, successive pathviewR functions can make use of these info, and
    ## also they will be available if the user wants to add them to a plot.

    ## Also, do you mind if we rename gnd_plane? We are using the ground
    ## plane as an approximator of the vertex location. So if we are to
    ## make things more generic (i.e. for other users) we could opt to
    ## rename this as vertex_height, vertex_relative_height, or something
    ## along those lines?


## Now coloring points by vis_angle_neg_deg
ggplot(full45, aes(x = position_width, y = position_height)) +
  geom_point(aes(color = vis_angle_neg_deg),
             shape = 1, size = 3)
# greater visual angles closer to negative (left) side of the tunnel

## And now for vertex_angle = 60
ggplot(full60, aes(x = position_width, y = position_height)) +
  geom_point(aes(color = vis_angle_neg_deg),
             shape = 1, size = 3)
# angain a subtle shift in distribution of angles




## Perhaps we're interested in the sum of the visual angles percieved by the
## bird
full45$vis_angle_sum <- full45$vis_angle_pos_deg + full45$vis_angle_neg_deg

ggplot(full45, aes(x = position_width, y = position_height)) +
  geom_point(aes(color = vis_angle_sum),
             shape = 1, size = 3)
# largest visual angles at the bottom of the tunnel



## Top view of the tunnel

## vis_angle_pos_deg
ggplot(full45, aes(x = position_length, y = position_width)) +
  geom_point(aes(color = vis_angle_pos_deg),
             shape = 1, size = 3)

## vis_angle_neg_deg
ggplot(full45, aes(x = position_length, y = position_width)) +
  geom_point(aes(color = vis_angle_neg_deg),
             shape = 1, size = 3)

## vis_angle_sum
ggplot(full45, aes(x = position_length, y = position_width)) +
  geom_point(aes(color = vis_angle_sum),
             shape = 1, size = 3)
# This view is not very helpful for displaying the sum because height is an
# important factor




          ##############    Next Steps    #############

##  1. Set gnd_plane automatically by using the distance from the bottom of the
##  "V" to (0,0,0) by referencing something within rotate_tunnel().
##
##  2. Set vertex_angle and stim_param arguments from an insert_treatments()
##  function elsewhere in the pipeline.
##
##    ## suggestion from VBB -- maybe these two would be good to add as
##    ## attributes, rather than as columns, since each is a constant and
##    ## therefore it may be annoying to have them as columns running the
##    ## length of the tibble? In any case, I agree -- insert_treatments()
##    ## would be a very logical place to add this stuff in.
##
##      ** external to calc_vis_angle - include ordientation coordinates in
##  plots such that each point represents position and head orientation
##
##  3. Include head orientation to calculate visual angles based on where the
##  bird is actually looking rather than assuming a fixed gaze.
##  This one could take a while I'm still wrapping my head around quaternions.
##
##  4. Figure out how to visualize or otherwise use the visual angles calculated
##  from these functions.
##    - I believe this variable can be the nucleus with which to design
##    functions that more accurately estimate optic flow by calculating how it
##    changes over time in various dimensions, i.e. how it increases or
##    decreases with the bird's movements.
##    See rough work below where I'm trying to calculate these running changes.
##    Perhaps a simple derivative function could be used rather than diff() as
##    it reduces the number of data points by 1 and then can't be plotted with
##    time_sec.



## Testing area for calc_vis_angle uses in optic flow calculations


expansion_pos <- diff(full45$vis_angle_pos_deg)/diff(full45$time_sec)
expansion_neg <- diff(full45$vis_angle_neg_deg)/diff(full45$time_sec)

plot(expansion_pos, expansion_neg)

ang <- diff(full45$vis_angle_pos_deg)
time <- diff(full45$time_sec)
vecsum <- sqrt(full45$position_width^2 +
               full45$position_length^2 +
               full45$position_height^2)
vel <- diff(vecsum)



plot(full45$time_sec, vel)
plot(full45$position_width,
     full45$vis_angle_neg_deg,
     col = 'red')
points(full45$position_width,
     full45$vis_angle_pos_deg,
     col = 'blue')

plot(full45$position_height,
     full45$vis_angle_pos_deg)

product <- full45$position_height * full45$position_width

plot(product,
     full45$vis_angle_pos_deg,
     col = 'red')
points(product,
       full45$vis_angle_neg_deg,
       col = 'blue')

plot(full45$position_width,
     full45$position_height)


plot(full45$frame,
     full45$vis_angle_neg_deg,
     col = full45$traj_id)

















