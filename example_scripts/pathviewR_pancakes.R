## Last updated: 2020-07-26 VBB

## Script for testing things out as functions are written and showcasing worked
## examples.

############################### package loading ################################
## Specify the packages you'll use in the script
packages <- c("devtools",
              "tidyverse",
              "readxl",
              "R.matlab",
              "ggthemes",
              "viridis",
              "gridExtra",
              "data.table",
              "RColorBrewer",
              "rgl",
              "plot3D")
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
## (EP) on my machine, R.matlab didn't auto-install/load like the others...


################################ script sourcing ###############################
## Now that we've package-ized things, we not longer use source() to load up
## our functions. Instead, we can load our pathviewR package via
## devtools::load_all(). See sec 2.8 in the R packages guide by Wickham & Bryan
## for more on this.

devtools::load_all()

################################# data import ##################################
## Using example file from Melissa
jul_29_path <-
  './inst/extdata/july-29_group-I_16-20.csv'

## New import function. Let VBB know if there are issues!!!
jul_29 <-
  read_motive_csv(jul_29_path, simplify_marker_naming = TRUE)

## Try without simplifying marker naming (should be no change for this file)
jul_29_unsimple <-
  read_motive_csv(jul_29_path, simplify_marker_naming = FALSE)

## Other data
sham_dat <-
  read_motive_csv("./inst/extdata/sham_data_set.csv")
sham_dat_unsimple <-
  read_motive_csv("./inst/extdata/sham_data_set.csv",
                  simplify_marker_naming = FALSE)
wing_rom <-
  read_motive_csv(
    "./inst/extdata/2020-03-07_Ard_her_20-317L_4jts_7wng_ROM_001.csv")
wing_rom_unsimple <-
  read_motive_csv(
    "./inst/extdata/2020-03-07_Ard_her_20-317L_4jts_7wng_ROM_001.csv",
    simplify_marker_naming = FALSE)
elsa_dat <-
  read_motive_csv(
    "./inst/extdata/feb-20_mixed-group_10-35_trunc.csv"
  )

#### __simplify_marker_naming examples ####
## Via names() we can see what the `simplify_marker_naming` argument does
names(jul_29)
names(jul_29_unsimple) # should be identical to the above
names(sham_dat) # no "Ard_her_20-317L_4jt7wing:" in marker names; RBs unaffected
names(sham_dat_unsimple) # Explicit naming
names(wing_rom) # Marker names only
names(wing_rom_unsimple) # Subject:marker explicit naming

## The idea is to allow users to opt for a simpler (marker-only) naming
## of variables IFF there is only one "subject" within the data. If
## multiple subjects each had markers, going for the simplify option is
## likely a very bad idea. We should remember to express this in the
## documentation.

names(elsa_dat) # simpilfy works out just fine with this one

################################## rename axes #################################
## I get confused by the axis definitions. So I use the `relabel_viewr_axes()`
## utility function to rename the variables

jul_29 <- relabel_viewr_axes(jul_29,
                             tunnel_length = "_z",
                             tunnel_width = "_x",
                             tunnel_height = "_y",
                             real = "_w")

## Note that we now have an auto-generated Help file via roxygen:
?relabel_viewr_axes

elsa_renamed <- relabel_viewr_axes(elsa_dat) # use all defaults

####################### gather data into simpler format ########################
## The gathering function will take all data from a given session and
## organize it so that all data of a given type are within one column, i.e.
## all position lengths are in Position_length, as opposed to separate
## length columns for each rigid body.

jul_29_gathered <- gather_tunnel_data(jul_29)
elsa_gathered <- gather_tunnel_data(elsa_renamed)


############################### rename subjects! ###############################
## Simple utility function to rename subjects via pattern replacement. Uses
## stringr::str_replace() under the hood, so please consult that function's Help
## file for more details on what the "pattern" and "replacement" arguments can
## entail (they have regex capability, but you can also keep it simple)

## I'll opt to overwrite jul_29_gathered with cleaner subject names
jul_29_subjectsrenamed <-
  rename_viewr_characters(jul_29_gathered,
                          target_column =  "subject", ## This is the default
                          pattern = " 002", ## No default here; must be entered
                          replacement = "") ## The default
## Using replacement = "" amounts to removing the "pattern" characters from
## the cells within the target_column. So the above removes " 002" from each
## subject name (if it was there)

## I called it rename_viewr_characters() instead of rename_viewer_subjects()
## because this can acutally be applied to any column. This flexibility could
## come in handy if the user wants to revise the naming system in the traj_id
## column made by separate_trajectories() or if they want to revise names within
## columns that are added on by something else (e.g. a "treatments" column)
##
## That all said, I'm not terribly fond of the name, so suggestions are welcome!


################################# trim outliers ################################
## Use trim_tunnel_outliers() to remove artifacts and other outlier data
## This function relies on the user supplying estimates of the min and max
## values that are acceptable as data on each axis. Data ouside these ranges
## are filtered out. Best to plot data beforehand and check!!

## Plotting first!! (commented out for ease of use)
  # ## Plot of length vs height for July 29
  # plot(jul_29_gathered$position_length,
  #      jul_29_gathered$position_height,
  #       asp=1)
  #    abline(v = -0.06) # length min
  #    abline(v = 2.6)   # length max
  #    abline(h = -0.25) # height min
  #    abline(h = 0.35)  # height max
  #  ## Length vs width
  #  ## Keeping width estimates very wide since crazy artifacts don't really
  #  ## manifest in this dimension
  #  plot(jul_29_gathered$position_length,
  #       jul_29_gathered$position_width,
  #       asp=1)
  #    abline(v = -0.06) # length min
  #    abline(v = 2.6)   # length max
  #    abline(h = 0.8)   # width max (very generous)
  #    abline(h = -0.8)  # width min (very generous)

## Now use trim_tunnel_outliers() to trim out artifacts
## Defaults were defined by what worked for July 29th

jul_29_trimmed <- trim_tunnel_outliers(jul_29_subjectsrenamed)

# ## We can check that it worked by plotting length vs. height...etc again
# plot(jul_29_trimmed$position_length,
#      jul_29_trimmed$position_height,
#      asp=1)

################################# rotate tunnel ################################
## Use rotate_tunnel() to do two things simulatenously:
## 1) align perches on either end
## 2) make the center of the tunnel (0,0,0) for (length, width, height). Height
## is now standardized by (approximate) perch height; values greater than 0 are
## above the perch and values less than 0 are below the perch level.

## IMPORTANT NOTE: ALL THE DEFAULT VALUES IN THE FUNCTION ARE BASED ON WHAT
## WORKS FOR JULY 29. THESE VALUES WILL (LIKELY) NEED TO BE CHANGED FOR OTHER
## DATA SETS!!
jul_29_rotated <-
  jul_29_trimmed %>% rotate_tunnel()

attributes(jul_29_rotated)


############################### standardize tunnel #############################
## An alternate to rotate_tunnel(), this version performs all the same steps
## as rotate_tunnel() but does so by using pre-labeled markers or rigid bodies.
##
## IMPORTANT NOTE: THE LENGTH VALUE OF "landmark_one" MUST BE LOWER (I.E. TO
## THE LEFT) THAND THE LENGTH VALUE OF "landmark_two". Otherwise, the function
## will reflect all position_width values, creating a mirror-image effect,
## before the subsequent rotation. I can look for a way to account for this, but
## won't prioritize it now since it can be simply handled by making sure that
## the perch with the lower length value is set to landmark_one

## See note above; perch2 needs to be used for landmark_one
elsa_standardized <- standardize_tunnel(elsa_gathered,
                                        landmark_one = "perch2",
                                        landmark_two = "perch1")

## To check that it worked, first plot the original (_gathered) data:
plot(elsa_gathered$position_length,
     elsa_gathered$position_width,
     asp = 1)
# ## identify() is amazing for figuring out which perch is which
# ## Make sure the previous plot() is still active and is the most
# ## recently-generated plot. Then use the following block to click inside that
# ## plot on points you want identified. Then hit "Esc" and they will be labeled
# identify(elsa_gathered$position_length,
#          elsa_gathered$position_width,
#          labels = elsa_gathered$subject,
#          plot = TRUE)

## Now plot the standardized data
plot(elsa_standardized$position_length,
     elsa_standardized$position_width,
     asp = 1)
# ## If you'd like to double-check, use identify() again to ensure the
# ## mirror-image issue doesn't arise.
# identify(elsa_standardized$position_length,
#          elsa_standardized$position_width,
#          labels = elsa_standardized$subject,
#          plot = TRUE)

## In this example, there's almost no rotation necessary. But do note that
## the center point is now set to (0, 0, 0), which is important for subsequent
## steps

############################### select x percent ###############################
## Select the middle X percent of the tunnel, according to the length
## dimension.
##
## Enter a number from 0 to 100 that corresponds to the percent of the total
## length of the tunnel that should be retained. That percent of data, starting
## from the middle, are retained. E.g. 50 retains half the tunnel

jul_29_selected <-
  jul_29_rotated %>% select_x_percent(50)


############################### label trajectories #############################
## From a selected tunnel, make a column that indicates the identity of each
## trajetory.
## Trajectories are defined as groups of data that are separated from each
## by a particular threshold. The max_frame_gap argument defines the largest
## permissible gap in data before a new trajectory is defined.
##
## Setting max_frame_gap = 1 means that a new trajectory is defined once one
## missing frame appears. max_frame_gap = 9 would define a new trajectory if
## a gap of 9 frames appears.

## Trajectories are defined when 5 frames in a row are missing
jul_29_labeled <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = 5)

# ## Plot with unique colors for combinations of rigid bodies and trajectories
# ## We run out of colors, so they're recycled. Interpret carefully...
plot(jul_29_labeled$position_length,
     jul_29_labeled$position_width,
     asp = 1, col = as.factor(jul_29_labeled$sub_traj))

## Or simply by rigid body ID
plot(jul_29_labeled$position_length,
     jul_29_labeled$position_width,
     asp = 1, col = as.factor(jul_29_labeled$subject))

## Or simply by trajectory ID
plot(jul_29_labeled$position_length,
     jul_29_labeled$position_width,
     asp = 1, col = as.factor(jul_29_labeled$traj_id))

#### __behavior of max_frame_gap ####
## Entering a numeric still allows it to behave as it did before (above example
## shows this already)

## You can also use it without needing to run through select_x_percent first:
jul_29_labeled_unselected <-
  jul_29_rotated %>% separate_trajectories(max_frame_gap = 1)
plot(jul_29_labeled_unselected$position_length,
     jul_29_labeled_unselected$position_width,
     asp = 1, col = as.factor(jul_29_labeled_unselected$traj_id))
## Note that it takes a bit longer if you're using a pre-select_x_percent object
## since there are many more rows. But the plot checks out!

## It also has safeguarding now to ensure that max_frame_gap does not exceed
## the actual maximum gap between frames throughout the data:
jul_29_labeled_overflow <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = 30000000000)
## Obviously, a max_frame_gap of 33296 is not ideal, either. But this ensures
## that the value will at least be one that is feasible, given the data. It
## also amounts to setting all the data as belonging to one trajectory:
plot(jul_29_labeled_overflow$position_length,
     jul_29_labeled_overflow$position_width,
     asp = 1, col = as.factor(jul_29_labeled_overflow$traj_id))

## A final neat trick among numeric entries for max_frame_gap is setting it to
## any value lower than 1. I haven't played around with this thoroughly, but
## max_frame_gap < 1 (including negative values) should make every frame a
## separate trajectory.
jul_29_labeled_negatory <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = -1)
plot(jul_29_labeled_negatory$position_length,
     jul_29_labeled_negatory$position_width,
     asp = 1, col = as.factor(jul_29_labeled_negatory$traj_id))
## I consider this a "cheat code" for cases in which it may make sense to treat
## each observation as a unique grouping factor. I don't exactly know why that
## would be good, but my spidey sense tells me it may prove useful someday...

## "autodetect" has been updated! It now:
## Splits the data by subject and computes a max_frame_gap for each subject
jul_29_labeled_autodetect <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = "autodetect")
plot(jul_29_labeled_autodetect$position_length,
     jul_29_labeled_autodetect$position_width,
     asp = 1, col = as.factor(jul_29_labeled_autodetect$traj_id))
## Frame gap values are reported in attributes:
  attr(jul_29_labeled_autodetect, "max_frame_gap")
## Use frame_gap_messaging = TRUE to get reports of selecte frame gaps
jul_29_labeled_autodetect <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = "autodetect",
                                            frame_gap_messaging = TRUE)
## Use frame_gap_plotting = TRUE to get elbow plots! One per subject
jul_29_labeled_autodetect <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = "autodetect",
                                            frame_gap_plotting = TRUE)

## THE visualize_frame_gap_choice() MAY NOT BE NECESSARY ANYMORE!
## Complementary visualization function, adapted from Melissa's determine_fg_M()
visualize_frame_gap_choice(jul_29_selected, loops = 25)
## Note that you'll get a different answer if a different loop length is used:
visualize_frame_gap_choice(jul_29_selected, loops = 20)
visualize_frame_gap_choice(jul_29_selected, loops = 50)

## We'll probably want to make a check for situtations in which character
## vectors are used which arent "autodetect". This is so we can guard against
## stuff like:
jul_29_labeled_steve <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = "steve")
plot(jul_29_labeled_steve$position_length,
     jul_29_labeled_steve$position_width,
     asp = 1, col = as.factor(jul_29_labeled_steve$traj_id))
## Then again, all trajectory labels are set to 0. So perhaps this task
## fails successfully!

########################### keep full trajectories #############################
## Once trajectories have been labeled, this next function will retain only the
## data from trajectories that satisify two conditions.
## 1) The trajectory must start at one end of the tunnel and end at the other
## 2) The trajectory must span beyond a specified proportion of the selected
## tunnel. For example, if span = 0.9, each trajectory must span at least 90%
## of the selected tunnel's length. I'd recommend not letting this fall below
## 0.8, but keep in mind that values closer to 1 will likely shave off data,
## too.

jul_29_full <-
  jul_29_labeled %>% get_full_trajectories(span = 0.95)
attr(jul_29_full, "pathviewR_steps")
plot(jul_29_full$position_length,
     jul_29_full$position_width,
     asp = 1, col = as.factor(jul_29_full$traj_id))

####################### compute instantaneous velocities #######################
## 2020-06-26: This revised function how has more flexiblity in what a user can
## do and when the user can choose to implement it. I'll play around in this
## section to see how it behaves and give some examples of how people may want
## to use it

## Appending velocity data as new columns on an exisiting viewr object
## The argument `add_to_viewr` is TRUE by default and will add columns
jul_29_full_with_velocity <-
  jul_29_full %>% get_velocity()
## But if you don't want a billion columns, go with a simple velocity output:
jul_29_full_only_velocity <-
  jul_29_full %>% get_velocity(add_to_viewr = FALSE)

## When can this function be used? At basically any point, I think:
jul_29_gathered_velocity <-
  jul_29_gathered %>% get_velocity()
jul_29_selected_velocity <-
  jul_29_selected %>% get_velocity()
jul_29_rotated_velocity <-
  jul_29_rotated %>% get_velocity()

## This flexiblity even allows for non-standard column naming, but the downside
## is that it can also lead to weird scenarios:
jul_29_unsimple_velocity <- ## using jul_29_unsimple bc it hasn't been relabeled
  jul_29_unsimple %>% get_velocity(time_col   = "time_sec",
                                   length_col = "device08_002_position_x",
                                   width_col  = "device08_002_position_y",
                                   height_col = "device08_002_position_z",
                                   add_to_viewr = TRUE)
## Now if you look at jul_29_unsimple_velocity, velocities were indeed
## calculated and appended, but we are limited to computing them for device08
## only.

## Also, error messages are designed to be as informative as possible, e.g.:
jul_29_unsimple_velocity_fail <-
  jul_29_unsimple %>% get_velocity()


######################### one function to rule them all ########################
## Let's see if we can make an all-in-one that behaves nicely when the user
## would like to specify non-default values to the arguments.

jul_29_path <- './inst/extdata/july-29_group-I_16-20.csv'

## Testing first with all defaults (no supplied arguments)
## this sets max_frame_gap to 1 (no autodetect)
jul_29_all_defaults <-
  jul_29_path %>% import_and_clean_viewr()
jul_29_all_defaults # seems to work
class(jul_29_all_defaults) # looks complete
attributes(jul_29_all_defaults)
plot(jul_29_all_defaults$position_length,
     jul_29_all_defaults$position_width,
     asp = 1, col = as.factor(jul_29_all_defaults$traj_id))

## plot each trajectory (this is extensive!)
## probably best to clear all your plots before running this:
trajs <- unique(jul_29_all_defaults$sub_traj)
for (i in 1:length(trajs)){
  tmp <- jul_29_all_defaults %>% filter(sub_traj == trajs[i])
  plot(tmp$position_length,
       tmp$position_width,
       asp = 1,
       ## add a title that indicates sub_traj
       main = trajs[i],
       ## keep the same dimensions across all plots:
       xlim = c(min(jul_29_all_defaults$position_length),
                max(jul_29_all_defaults$position_length)),
       ylim = c(min(jul_29_all_defaults$position_width),
                max(jul_29_all_defaults$position_width))
       )
}

## Testing now with autodetect
jul_29_mfg_autodetect <-
  jul_29_path %>% import_and_clean_viewr(max_frame_gap = "autodetect",
                                         frame_gap_messaging = TRUE)
jul_29_mfg_autodetect # seems to work
class(jul_29_mfg_autodetect) # looks complete
attributes(jul_29_mfg_autodetect)
plot(jul_29_mfg_autodetect$position_length,
     jul_29_mfg_autodetect$position_width,
     asp = 1, col = as.factor(jul_29_mfg_autodetect$traj_id))

## plot each trajectory (this is extensive!)
## probably best to clear all your plots before running this:
auto_trajs <- unique(jul_29_mfg_autodetect$sub_traj)
for (i in 1:length(auto_trajs)){
  tmp <- jul_29_mfg_autodetect %>% filter(sub_traj == auto_trajs[i])
  plot(tmp$position_length,
       tmp$position_width,
       asp = 1,
       ## add a title that indicates sub_traj
       main = auto_trajs[i],
       ## keep the same dimensions across all plots:
       xlim = c(min(jul_29_mfg_autodetect$position_length),
                max(jul_29_mfg_autodetect$position_length)),
       ylim = c(min(jul_29_mfg_autodetect$position_width),
                max(jul_29_mfg_autodetect$position_width))
  )
}
## OK something is broken here. Now I want to see what happens when done
## explicitly instead

## Testing now with autodetect
jul_29_mfg_auto_explicit <-
  jul_29_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  get_velocity() %>%
  select_x_percent() %>%
  ## skip rename_viewr_characters(), which defaults to FALSE anyway
  separate_trajectories(max_frame_gap = "autodetect",
                        frame_gap_messaging = TRUE) %>%
  get_full_trajectories()
jul_29_mfg_auto_explicit # seems to work
class(jul_29_mfg_auto_explicit) # looks complete
attributes(jul_29_mfg_auto_explicit)
identical(jul_29_mfg_autodetect, jul_29_mfg_auto_explicit)
plot(jul_29_mfg_auto_explicit$position_length,
     jul_29_mfg_auto_explicit$position_width,
     asp = 1, col = as.factor(jul_29_mfg_auto_explicit$traj_id))

## plot each trajectory (this is extensive!)
## probably best to clear all your plots before running this:
auto_expl_trajs <- unique(jul_29_mfg_auto_explicit$sub_traj)
for (i in 1:length(auto_expl_trajs)){
  tmp <- jul_29_mfg_auto_explicit %>% filter(sub_traj == auto_expl_trajs[i])
  plot(tmp$position_length,
       tmp$position_width,
       asp = 1,
       ## add a title that indicates sub_traj
       main = auto_expl_trajs[i],
       ## keep the same dimensions across all plots:
       xlim = c(min(jul_29_mfg_auto_explicit$position_length),
                max(jul_29_mfg_auto_explicit$position_length)),
       ylim = c(min(jul_29_mfg_auto_explicit$position_width),
                max(jul_29_mfg_auto_explicit$position_width))
  )
}








## Done explicitly:
jul_29_all_defaults_explicit <-
  jul_29_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  get_velocity() %>%
  select_x_percent() %>%
  ## skip rename_viewr_characters(), which defaults to FALSE anyway
  separate_trajectories() %>%
  get_full_trajectories()

## Are jul_29_all_defaults and jul_29_all_defaults_explicit the same?
identical(jul_29_all_defaults, jul_29_all_defaults_explicit)

## Let's try a different X% of the tunnel
jul_29_percent74 <-
  jul_29_path %>% import_and_clean_viewr(desired_percent = 74)
# works!
plot(jul_29_percent74$position_length,
     jul_29_percent74$position_width,
     asp = 1, col = as.factor(jul_29_percent74$traj_id))


## Done explicitly:
jul_29_percent74_explicit <-
  jul_29_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  get_velocity() %>%
  select_x_percent(desired_percent = 74) %>%
  separate_trajectories() %>%
  get_full_trajectories()

## Are jul_29_percent74 and jul_29_percent74_explicit the same?
identical(jul_29_percent74, jul_29_percent74_explicit)

## Third example, use span = 0.95 along with desired_percent = 74
jul_29_percent74_span95 <-
  jul_29_path %>% import_and_clean_viewr(desired_percent = 74, span = 0.95)

## Done explicitly:
jul_29_percent74_span95_explicit <-
  jul_29_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  get_velocity() %>%
  select_x_percent(desired_percent = 74) %>%
  separate_trajectories() %>%
  get_full_trajectories(span = 0.95)

## Are jul_29_percent74_span95 and jul_29_percent74_span95_explicit the same?
identical(jul_29_percent74_span95, jul_29_percent74_span95_explicit)
## noice noice noice!!!

## 2020-06-25: post re-write of the function, the above testing in this
## subsection still produces valid results. Yay! That said, I want to to test
## other parts of this. So what follows will be attempts to break how the
## all-in-one works to showcase its behavior. Suggestions very welcome!!

## Let's try a B.S. option for the standardization_option argument
jul_29_steve <-
  jul_29_path %>% import_and_clean_viewr(standardization_option = "steve")
## Fails! And it gives informative error messages!

## Let's try skipping the separate_trajectories step:
jul_29_skipseparate <-
  jul_29_path %>% import_and_clean_viewr(separate_trajectories = FALSE)
## Fails! And it gives an informative error message!

## Skip trimming tunnel outliers
jul_29_skiptrim <-
  jul_29_path %>% import_and_clean_viewr(trim_tunnel_outliers = FALSE)
## Seems to work!

## Skip trimming and get_full_trajectories
jul_29_skiptrim_and_full <-
  jul_29_path %>% import_and_clean_viewr(trim_tunnel_outliers = FALSE,
                                         get_full_trajectories = FALSE)
## Skipping two steps seems to work! Note, these two were chosen because doing
## so won't break anything.

## Obviously, skipping certain steps will break the pipeline or otherwise cause
## problems. But users should be tasked to think carefully about what they're
## implementing.

## Also, opting for `add_to_viewr = FALSE` in get_velocity() will break the
## pipeline right before select_x_percent().
jul_29_dontaddvelocity <-
  jul_29_path %>% import_and_clean_viewr(add_to_viewr = FALSE)

## Trying to resolve what Melissa mentioned in:
## https://github.com/vbaliga/pathviewR/issues/14
jul_29_alltest1 <-
  jul_29_path %>%
  import_and_clean_viewr(separate_trajectories = FALSE,
                         get_full_trajectories = FALSE,
                         max_frame_gap = 3,
                         span = 0.95,
                         steve = "boop")
## The function now stops and reports an explicit error message about:
## 1) any unrecognized arguments (here: steve); these are simply ignored
## 2) any unused arguments. The function stops and produces (what I hope is)
## an informative error

## Unnamed AND unrecognized arguments are silently ignored, though:
jul_29_alltest2 <-
  jul_29_path %>%
  import_and_clean_viewr("boop")



jul_29_test <-
  jul_29_path %>%
  import_and_clean_viewr(velocity_min = 1,
                         velocity_max = 10,
                         desired_percent = 75,
                         target_column = "subject",
                         pattern = " 00.",
                         replacement = "",
                         max_frame_gap = "autodetect",
                         frame_gap_messaging = TRUE,
                         frame_gap_plotting = FALSE,
                         span = .6)
#testing get_full_trajs:
summary_obj <-
  jul_29_test %>%
  dplyr::group_by(sub_traj) %>%
  dplyr::summarise(traj_length = n(),
                   start_length = position_length[1],
                   end_length = position_length[traj_length],
                   length_diff = abs(end_length - start_length),
                   start_length_sign = sign(start_length),
                   end_length_sign = sign(end_length))
test <- filter(jul_29_test, sub_traj == "device08_1")


jul_29_defaults <-
  jul_29_path %>%
  import_and_clean_viewr(target_column = "subject",
                         pattern = " 00.",
                         replacement = "")
jul_29_rename <- jul_29_defaults %>%
  rename_viewr_characters(pattern = " 00.")

jul_29_sep <-
  jul_29_rename %>%
  separate_trajectories(max_frame_gap = "autodetect",
                        frame_gap_messaging = TRUE,
                        frame_gap_plotting = TRUE)
attributes(jul_29_sep)

jul_29_full <-
  jul_29_sep %>%
  get_full_trajectories(span = .6)

jul_29_test %>%
  mutate(traj_id = as.character(sub_traj)) %>%
  ggplot(aes(position_length, position_height, color = sub_traj)) +
  geom_point() +
  scale_color_viridis_d() +
  theme(legend.position = "none")

  #################################### roz2016 ###################################
## Going to start adding things to help me integrate flydra data into this
## package.

## rhdf5 is on Bioconductor and needed to open H5 files
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.11")
# BiocManager::install("rhdf5")

library(rhdf5)
## import metadata from H5 files
## rhdf5::h5ls() is used to determine how H5 files are structured
  ex1_h5_ls <-
    rhdf5::h5ls("./inst/extdata/roz2016/DATA20160619_124428.h5")
  ex1_h5_kalmanized_ls <-
    rhdf5::h5ls("./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5")
  ex1_h5_smoothcache_ls <-
    rhdf5::h5ls(
      "./inst/extdata/roz2016/DATA20160619_124428.kalmanized.kh5-smoothcache")
## the .mat file can be read via R.matlab::readMat()
  ex1_h5_mat <-
    R.matlab::readMat(
      "./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5-short-only.mat")

## So observations are 33294 rows long but kalman vars are 33066. Why? Where
## does this difference of 228 rows arise?
  plot(ex1_h5_mat$kalman.x[1:33066], ex1_h5_mat$kalman.y[1:33066])
  plot(ex1_h5_mat$observation.x[1:33294], ex1_h5_mat$observation.y[1:33294])
## Comparing these two plots makes me think that two things were done:
## 1) Trajectories were smoothed via a kalman filter (duh)
## 2) Gaps in trajectories were also filled via said filter. Will need to think
## about whether I agree with doing this. Guess it's ok for now.
##
## Accordingly, the sizes of the two data sets need not be identical. And even
## more importantly, there won't be an a priori expectation of how the two sets
## differ in length -- rather, it will be an emergent property of how & when the
## trajectory gaps arose. Oof.

## Also:
  sum(as.numeric(ex1_h5_smoothcache_ls$dim[-1])) # equals 33066!!

## Some more fun with metadata
## The rows within ex1_h5_ls$name provide the names of grouped metadata
## This info can be used in conjunction with rhdf5::h5read() to formally
## import these metadata, e.g.
  ex1_cam_info <- rhdf5::h5read("./inst/extdata/roz2016/DATA20160619_124428.h5",
                                name = "cam_info")
  ex1_data2d <- rhdf5::h5read("./inst/extdata/roz2016/DATA20160619_124428.h5",
                              name = "data2d_distorted") #this one fails :(

  ex1_kalmanized_trigger_clock <-
    rhdf5::h5read('./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5',
                  name = "trigger_clock_info")

  ex1_kalmanized_textlog <-
    rhdf5::h5read('./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5',
                  name = "textlog")

## Thoughts on next steps:
## A read_flydra_mat() function can be written to use R.matlab::readMat() to
## import the .MAT file. The .MAT file does not contain all the necessary
## metadata; these will need to be imported from the accompanying H5 files.
##
## An as_viewr() function can be written in parallel that is inspired by the
## way read_flydra_mat() is composed. An end user will likely have data
## organized as a data.frame or tibble already, but what they will need explicit
## instruction on is what & how metadata should be included & formatted.

## Let's plot in 3D
rgl::plot3d(x = ex1_h5_mat$kalman.x,
            y = ex1_h5_mat$kalman.y,
            z = ex1_h5_mat$kalman.z)
aspect3d("iso")
## Perches seems to be on oppose ends of x-axis and z-axis seems to correspond
## to height (up vs. down).

## 2D plot -- using this to estimate perch height for now
plot(x = ex1_h5_mat$kalman.x,
     y = ex1_h5_mat$kalman.z,
     asp = 1)
abline(h = 1.44)

#### __testing pathviewR flydra functions ####
## Test pancake
test_mat <-
  read_flydra_data(
    "./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5-short-only.mat",
    subject_name = "steve")

#attributes(test_mat)
attr(test_mat,"header")
attr(test_mat,"pathviewR_steps")

## Where this bring us:
## The way data were exported from flydra gives us the ability to skip over the
## relabel_viewr_axes() and gather_tunnel_data() steps. I am also fairly
## confident that we can skip the trim_tunnel_outliers() and most of the
## rotate/standardize_tunnel() steps. That said, an important feature of the
## rotate/standardize_tunnel() functions is that the coordinates are shifted so
## that (0, 0, 0) is at the center of the data, which then sets up subsequent
## functions like select_x_percent() to work from the middle of the tunnel
## outwards. Using select_x_percent() on the flydra data now amounts to the
## following:

## Import data and proceed directly to select_x_percent()
test_selected <-
  test_mat %>%
  select_x_percent(desired_percent = 50)

## Full (non-selected) data plot:
rgl::plot3d(x = test_mat$position_length,
            y = test_mat$position_width,
            z = test_mat$position_height)
aspect3d("iso")

## Post-select_x_percent()
rgl::plot3d(x = test_selected$position_length,
            y = test_selected$position_width,
            z = test_selected$position_height)
aspect3d("iso")
## Because length = 0 is at one perch (one extreme end of the tunnel),
## select_x_percent() clips the data incorrectly.

## SO, that means that the flydra data will need to be standardized
## such that (0, 0, 0) is the center of the data. This is probably easily done
## for the _length and _width axes, but _height may take some thinking. I
## believe (/ am very much hoping!) that the perch heights were written down in
## Roz's notebook. We could then set the perch height to equal 0, thereby
## leaving us with positive values indicating position above the perch level and
## negative values indicating positions below perch level.

#### __centering function ####
test_centered <-
  test_mat %>%
  redefine_tunnel_center(length_method = "middle",
                         height_method = "user-defined",
                         height_zero = 1.44)

#open3d() ## if needed
rgl::plot3d(x = test_centered$position_length,
            y = test_centered$position_width,
            z = test_centered$position_height)
aspect3d("iso")
## Hot damn! It worked!!

## Let's see if we can plug into select_x_percent() now
test_selected <-
  test_centered %>%
  select_x_percent(desired_percent = 50)
## 3D plot
rgl::plot3d(x = test_selected$position_length,
            y = test_selected$position_width,
            z = test_selected$position_height)
aspect3d("iso")

## Next steps
test_full <-
  test_selected %>%
  separate_trajectories(max_frame_gap = 1) %>%
  get_full_trajectories(span = 0.95)
## 3D plot
rgl::plot3d(x = test_full$position_length,
            y = test_full$position_width,
            z = test_full$position_height)
aspect3d("iso")
## 2D overhead
plot(test_full$position_length,
     test_full$position_width,
     asp = 1, col = as.factor(test_full$traj_id))


##### Quaternions maybe? #####
library(RSpincalc)

# S <- 1
# L <- 52 # length; using first trajectory only (rightwards)
S <- 53
L <- 98 #(leftwards)

test_pos <- data.frame(position_length = jul_29_full$position_length[S:L],
                       position_width =  jul_29_full$position_width[S:L],
                       position_height = jul_29_full$position_height[S:L])

rgl::plot3d(test_pos$position_length,
            test_pos$position_width,
            test_pos$position_height,
            ylim = c(-0.5, 0.5),
            zlim = c(-0.5, 0));aspect3d("iso")

test_quat <- cbind(jul_29_full$rotation_real[S:L],
                   jul_29_full$rotation_length[S:L],
                   jul_29_full$rotation_width[S:L],
                   jul_29_full$rotation_height[S:L])

test_quat2eulangl <- RSpincalc::Q2EA(test_quat, EulerOrder = "xyz")
test_quat2eulangl_deg <- apply(test_quat2eulangl, MARGIN = 2, FUN = rad2deg)
test_quat2eulvec <- RSpincalc::Q2EV(test_quat)


alltogether <- data.frame(position_length = jul_29_full$position_length[S:L],
                          position_width =  jul_29_full$position_width[S:L],
                          position_height = jul_29_full$position_height[S:L],
                          jul_29_full$rotation_real[S:L],
                          jul_29_full$rotation_length[S:L],
                          jul_29_full$rotation_width[S:L],
                          jul_29_full$rotation_height[S:L],
                          test_quat2eulangl_deg,
                          test_quat2eulvec)

lattice::splom(alltogether[,c(1:3,11:13)])


alltogether$len_trans <- alltogether$position_length + (alltogether$X1.1/3)
alltogether$wid_trans <- alltogether$position_width + (alltogether$X2.1/3)
alltogether$hei_trans <- alltogether$position_height + (alltogether$X3.1/3)



as.matrix(test_pos) -> M
colnames(M) <- NULL

as.matrix(cbind(alltogether$len_trans,
                alltogether$wid_trans,
                alltogether$hei_trans)) -> Tr


rgl::plot3d(x = M[,1],
            y = M[,2],
            z = M[,3],
            xlim = c(-1, 1),
            ylim = c(-0.5, 0.5),
            zlim = c(-0.5, 0.5));aspect3d("iso")
arrow3d(M[1,1:3], Tr[1,1:3], type = "lines", col = "red")
arrow3d(M[10,1:3], Tr[10,1:3], type = "lines", col = "orange")
arrow3d(M[15,1:3], Tr[15,1:3], type = "lines", col = "yellow")
arrow3d(M[20,1:3], Tr[20,1:3], type = "lines", col = "green")
arrow3d(M[30,1:3], Tr[30,1:3], type = "lines", col = "blue")
arrow3d(M[35,1:3], Tr[35,1:3], type = "lines", col = "grey30")
arrow3d(M[36,1:3], Tr[36,1:3], type = "lines", col = "black")
arrow3d(M[37,1:3], Tr[37,1:3], type = "lines", col = "grey70")
arrow3d(M[50,1:3], Tr[50,1:3], type = "lines", col = "purple")
