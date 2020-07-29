## Last updated: 2020-07-28 VBB

## Script for testing things out as functions are written and showcasing worked
## examples.

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

################################ script sourcing ###############################
## devtools::load_all() ensures that functions housed in /R but for which we
## have not written roxygen documentation are still loaded and avaliable to us
devtools::load_all()


################################# data import ##################################
## Using example file from Melissa
jul_29_path <-
  './inst/extdata/july-29_group-I_16-20.csv'

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
jul_29 <- relabel_viewr_axes(jul_29,
                             tunnel_length = "_z",
                             tunnel_width = "_x",
                             tunnel_height = "_y",
                             real = "_w")

elsa_renamed <- relabel_viewr_axes(elsa_dat) # use all defaults


####################### gather data into simpler format ########################
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
                          pattern = " 00.", ## No default here; must be entered
                          replacement = "") ## The default
## Using replacement = "" amounts to removing the "pattern" characters from
## the cells within the target_column. So the above removes " 002" from each
## subject name (if it was there)


################################# trim outliers ################################
## Defaults were defined by what worked for July 29th
jul_29_trimmed <- trim_tunnel_outliers(jul_29_subjectsrenamed)


################################# rotate tunnel ################################
## Use rotate_tunnel() to do two things simulatenously:
## 1) align perches on either end
## 2) make the center of the tunnel (0,0,0) for (length, width, height). Height
## is now standardized by (approximate) perch height; values greater than 0 are
## above the perch and values less than 0 are below the perch level.

## IMPORTANT NOTE: ALL THE DEFAULT VALUES IN THE FUNCTION ARE BASED ON WHAT
## WORKS FOR JULY 29.
jul_29_rotated <-
  jul_29_trimmed %>% rotate_tunnel()


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
## Append velocity data as new columns on an exisiting viewr object. Can be done
## anytime, but highly recommended it at least be after the gather step
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
trajs <- unique(jul_29_all_defaults$file_sub_traj)
for (i in 1:length(trajs)){
  tmp <- jul_29_all_defaults %>% filter(file_sub_traj == trajs[i])
  plot(tmp$position_length,
       tmp$position_width,
       asp = 1,
       ## add a title that indicates file_sub_traj
       main = trajs[i],
       ## keep the same dimensions across all plots:
       xlim = c(min(jul_29_all_defaults$position_length),
                max(jul_29_all_defaults$position_length)),
       ylim = c(min(jul_29_all_defaults$position_width),
                max(jul_29_all_defaults$position_width))
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


#################################### roz2016 ###################################

#### __testing pathviewR flydra functions ####
## Test pancake
test_mat <-
  read_flydra_mat(
    "./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5-short-only.mat",
    subject_name = "steve")

#attributes(test_mat)
attr(test_mat,"header")
attr(test_mat,"pathviewR_steps")

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
library(scales)
n <- length(unique(test_full$file_sub_traj))
dat.col <- data.frame(grps = unique(test_full$file_sub_traj),
                      colz = rainbow(n))  ## you can also use  brewer_pal()(n)
trj <- tibble(grps = test_full$file_sub_traj)
data_col <- left_join(trj, dat.col)
rgl::plot3d(x = test_full$position_length,
            y = test_full$position_width,
            z = test_full$position_height,
            col = data_col$colz)

## 2D overhead
plot(test_full$position_length,
     test_full$position_width,
     asp = 1, col = as.factor(test_full$file_sub_traj))


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
