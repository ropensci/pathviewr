## Last updated: 2020-06-12 VBB

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


################################ script sourcing ###############################
## Now that we've package-ized things, we not longer use source() to load up
## our functions. Instead, we can load our pathviewR package via
## devtools::load_all(). See sec 2.8 in the R packages guide by Wickham & Bryan
## for more on this.

devtools::load_all()

## Now all of our functions are available to us. Pretty nifty

################################# data import ##################################
## Using example file from Melissa

## Copied from below, for convenience
jul_29_path <- './inst/extdata/july-29_group-I_16-20.csv'

jul_29 <- read_motiv_csv('./inst/extdata/july-29_group-I_16-20.csv')
## Note: as of 2020-06-03 we are no longer coercing objects into the class
## 'motiv'. This is because of a change to tidyverse, which makes it hard
## to use non-tibble objects in its functions. Although our functions had
## made objects that had multple classes (namely, tibbles as well as motiv
## objects), the way class hierarchy is now handled makes it all more
## complicated to pass through tidyverse. I'm pretty salty about this, but
## it is what it is. Anyway, I'll start storing our steps in the attributes
## of the objects instead of classes -- hopefully this will work out better.
## To demonstrate:
class(jul_29) # should be a tibble
attr(jul_29, "pathviewR_steps") # "motiv"

## New import function
## Not yet ready for primetime, but adding it here so I can play around
## with it
jul_29 <- read_motive_csv(jul_29_path, simplify_marker_naming = TRUE)
jul_29_unsimple <- read_motive_csv(jul_29_path, simplify_marker_naming = FALSE)
sham_dat <- read_motive_csv("./inst/extdata/sham_data_set.csv")
sham_dat_unsimple <- read_motive_csv("./inst/extdata/sham_data_set.csv",
                                    simplify_marker_naming = FALSE)
wing_rom <-
  read_motive_csv("./inst/extdata/2020-03-07_Ard_her_20-317L_4jts_7wng_ROM_001.csv")
wing_rom_unsimple <-
  read_motive_csv("./inst/extdata/2020-03-07_Ard_her_20-317L_4jts_7wng_ROM_001.csv", simplify_marker_naming = FALSE)

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

################################## rename axes #################################
## I get confused by the axis definitions. So I use the `relabel_motiv_axes()`
## utility function to rename the variables

jul_29 <- relabel_motiv_axes(jul_29,
                             tunnel_length = "_Z",
                             tunnel_width = "_X",
                             tunnel_height = "_Y")

## Note that we now have an auto-generated Help file via roxygen:
?relabel_motiv_axes

####################### gather data into simpler format ########################
## The gathering function will take all data from a given session and
## organize it so that all data of a given type are within one column, i.e.
## all position lengths are in Position_length, as opposed to separate
## length columns for each rigid body.

## Best practice right now is to drop all NAs first (preceding section) and
## then run the gather function. Otherwise there may be issues with sorting,
## filtering, and gathering data.

## EDIT on 2020-03-25 Let's actually try dropping NAs after gathering. I think
## it might work out ok?

jul_29_gathered <- gather_tunnel_data(jul_29)


################################# trim outliers ################################
## Use trim_tunnel_outliers() to remove artifacts and other outlier data
## This function relies on the user supplying estimates of the min and max
## values that are acceptable as data on each axis. Data ouside these ranges
## are filtered out. Best to plot data beforehand and check!!

## Plotting first!! (commented out for ease of use)
  # ## Plot of length vs height for July 29
  plot(jul_29_gathered$Position_lengths,
       jul_29_gathered$Position_heights,
        asp=1)
     abline(v = -0.06) # length min
     abline(v = 2.6)   # length max
     abline(h = -0.25) # height min
     abline(h = 0.35)  # height max
   ## Length vs width
   ## Keeping width estimates very wide since crazy artifacts don't really
   ## manifest in this dimension
   plot(jul_29_gathered$Position_lengths,
        jul_29_gathered$Position_widths,
        asp=1)
     abline(v = -0.06) # length min
     abline(v = 2.6)   # length max
     abline(h = 0.8)   # width max (very generous)
     abline(h = -0.8)  # width min (very generous)

## Now use trim_tunnel_outliers() to trim out artifacts
## Defaults were defined by what worked for July 29th

jul_29_trimmed <- trim_tunnel_outliers(jul_29_gathered)

## Can check that it worked by plotting length vs. height...etc again
## Won't show that here for brevity

plot(jul_29_trimmed$Position_lengths,
     jul_29_trimmed$Position_heights,
     asp=1)

################################# rotate tunnel ################################
## Use rotate_tunnel() to do two things simulatenously:
## 1) align perches on either end
## 2) make the center of the tunnel (0,0,0) for (length, width, height)
##
## 2020-02-20 Height is now standardized by (approximate) perch height; values
## greater than 0 are above the perch and values less than 0 are below the
## perch level.

## IMPORTANT NOTE: ALL THE DEFAULT VALUES IN THE FUNCTION ARE BASED ON WHAT
## WORKS FOR JULY 29. THESE VALUES WILL (LIKELY) NEED TO BE CHANGED FOR OTHER
## DATA SETS!!
jul_29_rotated <-
  jul_29_trimmed %>% rotate_tunnel()

attributes(jul_29_rotated)


############################### select X percent ###############################
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
##
## Trajectory IDs are stored in the "traj_id" column.
##
## EDIT 2020-03-25 $rb_traj now has concatenation of rigid body IDs and
## trajectory IDs

## Trajectories are defined when 5 frames in a row are missing
jul_29_labeled <-
  jul_29_selected %>% separate_trajectories(max_frame_gap = 5)

# ## Plot with unique colors for combinations of rigid bodies and trajectories
# ## We run out of colors, so they're recycled. Interpret carefully...
plot(jul_29_labeled$Position_lengths,
     jul_29_labeled$Position_widths,
     asp = 1, col = as.factor(jul_29_labeled$rb_traj))

## Or simply by rigid body ID
plot(jul_29_labeled$Position_lengths,
     jul_29_labeled$Position_widths,
     asp = 1, col = as.factor(jul_29_labeled$rigid_body))

## Or simply by trajctory ID
plot(jul_29_labeled$Position_lengths,
     jul_29_labeled$Position_widths,
     asp = 1, col = as.factor(jul_29_labeled$traj_id))


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

######################### one function to rule them all ########################
## Let's see if we can make an all-in-one that behaves nicely when the user
## would like to specify non-default values to the arguments.

jul_29_path <- './inst/extdata/july-29_group-I_16-20.csv'

## Testing first with all defaults (no supplied arguments)
jul_29_all_defaults <-
  jul_29_path %>% import_and_clean_motiv()
jul_29_all_defaults # seems to work
class(jul_29_all_defaults) # looks complete
attributes(jul_29_all_defaults)

## Let's try a different X% of the tunnel
jul_29_percent74 <-
  jul_29_path %>% import_and_clean_motiv(desired_percent = 74)
jul_29_percent74 # exists
class(jul_29_percent74) # looks complete
attributes(jul_29_percent74)

## plot length vs. width
  plot(jul_29_percent74$Position_lengths,
       jul_29_percent74$Position_widths,
       asp = 1, col = as.factor(jul_29_percent74$rigid_body))

## Change the X% and span parameters
jul_29_percent74_span0.95 <-
  jul_29_path %>% import_and_clean_motiv(desired_percent = 74,
                                        span = 0.95)
jul_29_percent74_span0.95 # exists
class(jul_29_percent74_span0.95) # looks complete
attributes(jul_29_percent74_span0.95)


## Note: lookup table code intentionally removed on 2020-06-02 because it
## will need to be re-written for the way we'll organize the package.




#################################### roz2016 ###################################
## Going to start adding things to help me integrate flydra data into this
## package.

## rhdf5 is on Bioconductor and needed to open H5 files
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.11")
# BiocManager::install("rhdf5")

library(rhdf5)
## import H5 file
  ex1_h5 <-
    rhdf5::h5ls("./inst/extdata/roz2016/DATA20160619_124428.h5")
  ex1_h5_kalmanized <-
    rhdf5::h5ls("./inst/extdata/roz2016/DATA20160619_124428.kalmanized.h5")
  ex1_h5_smoothcache <-
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
  sum(as.numeric(ex1_h5_smoothcache$dim[-1])) # equals 33066!!
