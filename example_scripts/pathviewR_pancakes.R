## Last updated: 2020-06-25 VBB

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


################################ script sourcing ###############################
## Now that we've package-ized things, we not longer use source() to load up
## our functions. Instead, we can load our pathviewR package via
## devtools::load_all(). See sec 2.8 in the R packages guide by Wickham & Bryan
## for more on this.

devtools::load_all()

## Now all of our functions are available to us. Pretty nifty

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

jul_29_trimmed <- trim_tunnel_outliers(jul_29_gathered)

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
## EDIT 2020-06-17 $sub_traj now has concatenation of subject IDs and
## trajectory IDs (formerly $rb_traj)

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

## Or simply by trajctory ID
plot(jul_29_labeled$position_length,
     jul_29_labeled$position_width,
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
  jul_29_path %>% import_and_clean_viewr()
jul_29_all_defaults # seems to work
class(jul_29_all_defaults) # looks complete
attributes(jul_29_all_defaults)

## Done explicitly:
jul_29_all_defaults_explicit <-
  jul_29_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  select_x_percent() %>%
  separate_trajectories() %>%
  get_full_trajectories()

## Are jul_29_all_defaults and jul_29_all_defaults_explicit the same?
identical(jul_29_all_defaults, jul_29_all_defaults_explicit)

## Let's try a different X% of the tunnel
jul_29_percent74 <-
  jul_29_path %>% import_and_clean_viewr(desired_percent = 74)
# works!

## Done explicitly:
jul_29_percent74_explicit <-
  jul_29_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
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

## 2020-06-23
## Let's plot in 3D
rgl::plot3d(x = ex1_h5_mat$kalman.x,
            y = ex1_h5_mat$kalman.y,
            z = ex1_h5_mat$kalman.z,
            aspect = 1)
## Perches seems to be on oppose ends of x-axis and z-axis seems to correspond
## to height (up vs. down).

## Let's start making what will ultimately be the flydra import function. This
## should be considered a work in progress until it is imported into the
## data_import_functions.R script. Porting it over there will imply that it has
## reached sufficient maturity.

read_flydra_data <-
  function(mat_file,
           file_id = NA,
           subject_name,
           ...) {

    ## Import checks
    if (missing(mat_file))
      stop("A mat_file is required")
    if (!file.exists(mat_file))
      stop(paste0("File ", mat_file, " not found!"))

    ## For now, we will assume that only one subject (one individual
    ## hummingbird) is present in the data. Since these subject names were not
    ## stored in the flydra data or accompanying H5 files (as far as I can see)
    ## this will need to be supplied by the user
    if (missing(subject_name))
      stop("A subject_name is required")

    ## Match file_id to mat_file if no file_id is supplied
      if (is.na(file_id)) file_id <-
          basename(mat_file)

    ## Get maketime of file (may not be accurate...use with caution!)
      mtime <-
        file.info(mat_file)$mtime

    ## Read the MAT file via R.matlab::readMat()
      mat_read <-
        R.matlab::readMat(mat_file)

    ## The data we'd like to tibble-ize is spread across various components
    ## of the list. We need to put it together manually.

    ## First get the dimensions of the data
      data_length <- length(mat_read$kalman.y)

    ## Now put the data together
      data <-
        tibble(
          # using kalman frame instead of observed frame
          frame = mat_read$kalman.frame,
          ## I actally don't know the time intervals yet, so I am just putting
          ## in a dummy sequence.
          time_sec = seq(from = 0, to = (data_length - 1), by = 1),
          subject = subject_name,
          position_length = mat_read$kalman.x,
          position_width = mat_read$kalman.y,
          position_height = mat_read$kalman.z
        )

    ## Add metadata as attributes()
    attr(data, "pathviewR_steps") <-
      c("viewr", "renamed_tunnel", "gathered_tunnel")
      ## Adding "renamed_tunnel" and "gathered" because axes are renamed as the
      ## tibble is being created above and we are basically already in gathered
      ##  format.
    attr(data, "file_id") <- file_id
    attr(data, "file_mtime") <- mtime
      ## We will opt to store the original matlab file as an attribute since
      ## it very likely contains things we may need later. Hard to say what
      ## exactly right now; this is motivated by spidey-sense...
    attr(data, "flydra_mat") <- mat_read
    attr(data, "header") <- attr(mat_read, "header")

    ## Export
    return(data)
  }

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
            z = test_mat$position_height,
            aspect = 1)

## Post-select_x_percent()
rgl::plot3d(x = test_selected$position_length,
            y = test_selected$position_width,
            z = test_selected$position_height,
            aspect = 1)
## Because length = 0 is at one perch (one extreme end of the tunnel),
## select_x_percent() clips the data incorrectly.

## SO, that means that the flydra data will need to be standardized
## such that (0, 0, 0) is the center of the data. This is probably easily done
## for the _length and _width axes, but _height may take some thinking. I
## believe (/ am very much hoping!) that the perch heights were written down in
## Roz's notebook. We could then set the perch height to equal 0, thereby
## leaving us with positive values indicating position above the perch level and
## negative values indicating positions below perch level.

