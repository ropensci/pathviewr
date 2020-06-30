## Part of the pathviewR package
## Last updated: 2020-06-25 MSA & VBB

############################### relabel_viewr_axes #############################

#' Relabel the dimensions as length, width, and height
#'
#' Axes are commonly labeled as "x", "y", and "z" in recording software yet
#' \code{pathviewR} functions require these to be labeled as "length", "width",
#' and "height". \code{relabel_viewr_axes()} is a function that takes a
#' \code{viewr} object and allows the user to rename its variables.
#'
#' @param obj_name A tibble or data.frame with attribute \code{viewr}
#' @param tunnel_length The dimension that corresponds to tunnel length. Set to
#' \code{tunnel_length = "_z"} by default. Argument should contain a character
#' vector with a leading underscore (see Details)
#' @param tunnel_width The dimension that corresponds to tunnel width. Follows
#' the same conventions as \code{tunnel_length} and defaults to
#' \code{tunnel_length = "_x"}
#' @param tunnel_height The dimension that corresponds to tunnel height. Follows
#' the same conventions as \code{tunnel_length} and defaults to
#' \code{tunnel_length = "_y"}
#' @param real The dimension that corresponds to the "real" parameter in
#' quaternion notation (for data with "rotation" values). Follows the same
#' conventions as \code{tunnel_length} and defaults to \code{real = "_w"}
#' @param ... Additional arguments to be passed to \code{read_motive_csv()}.
#'
#' @details Each argument must have a leading underscore ("_") and each
#' argument must have an entry. E.g. tunnel_length = "_Y" will replace all
#' instances of _Y with _length in the names of variables.
#'
#' @return A tibble or data.frame with variables that have been renamed.
#'
#' @author Vikram B. Baliga
#'
#' @family utility functions
#'
#' @examples
#'
#' library(pathviewR)
#'
#' ## Import the july 29 example data included in the package
#' jul_29 <-
#'   read_motive_csv(system.file("extdata", "july-29_group-I_16-20.csv",
#'                              package = 'pathviewR'))
#'
#' ## Names of variables are labeled with _x, _y, _z, which we'd like to rename
#' names(jul_29)
#'
#' ## Now use relabel_viewr_axes() to rename these variables using _length,
#' _width, and _height instead
#' jul_29_relabeled <-
#'   relabel_viewr_axes(jul_29,
#'                      tunnel_length = "_z",
#'                      tunnel_width = "_x",
#'                      tunnel_height = "_y",
#'                      real = "_w")
#'
#' ## See the result
#' names(jul_29_relabeled)
#'
#' @seealso
#' \code{\link{read_motive_csv}}
#'
#' @export


relabel_viewr_axes <- function(obj_name,
                               tunnel_length = "_z",
                               tunnel_width = "_x",
                               tunnel_height = "_y",
                               real = "_w",
                               ...){
  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Inputs should be character vectors
  if (!is.character(tunnel_length)) {
    stop("tunnel_length should be a character vector")
  }
  if (!is.character(tunnel_width)) {
    stop("tunnel_width should be a character vector")
  }
  if (!is.character(tunnel_height)) {
    stop("tunnel_height should be a character vector")
  }

  namez <- names(obj_name)
  # THE FOLLOWING STEP MUST COME BEFORE WIDTH RENAMING! This is because the
  # 'real' dimension in quaternion notation is denoted "w", which conflicts
  # with the "w" in width
  namez <- sub(real,"_real",namez)
  namez <- sub(tunnel_width,"_width",namez)
  namez <- sub(tunnel_length,"_length",namez)
  namez <- sub(tunnel_height,"_height",namez)

  ## Note for future selves: The above assumes that X, Y, and Z apply in the
  ## same way to rotations as they do for positions. I am not familiar enough
  ## with quaternion notation yet to say. Should e.g. rotation_x not cleanly
  ## correspond to rotation along the "length" dimension of the tunnel, the
  ## above can be made more specific via:
  ## 1) changing arguments of this function to e.g.
  ## tunnel_length = "position_z"
  ## 2) changing sub() behavior to: sub(tunnel_length,"position_length",namez)

  ## Assign names
  namez -> names(obj_name)

  ## Leave a note that the axes have been renamed
  attr(obj_name,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "renamed_tunnel")

  ## Export
  return(obj_name)
}


############################# gather_tunnel_data ###############################

#' Gather data columns into key-value pairs
#'
#' Reformat \code{viewr} data into a "tidy" format so that every row corresponds
#' to the position (and potentially rotation) of a single subject during an
#' observed frame and time.
#'
#' @param obj_name A tibble or data.frame with attribute \code{viewr} that has
#' ideally been passed through \code{relabel_viewr_axes()}. See Details for
#' formatting requirements.
#' @param NA_drop Should rows with NAs be dropped? Defaults to \code{TRUE}
#' @param ... Additional arguments that can be passed to other \code{pathviewR}
#' functions such as \code{relabel_viewr_axes()} or \code{read_motive_csv()}
#'
#' @details The tibble or data.frame that is fed in must have variables that
#' have subject names and axis names separated by underscores. Axis names must
#' be one of the following: \code{position_length}, \code{position_width}, or
#' \code{position_height}. Each of these three dimensions must be present in the
#' data. Collectively, this means that names like \code{bird01_position_length}
#' or \code{larry_position_height} are acceptible, but \code{bird01_x} or
#' \code{bird01_length} are not.
#'
#' @return A tibble in "tidy" format which is formatted to have every row
#' correspond to the position (and potentially rotation) of a single subject
#' during an observed frame and time. Subjects' names are automatically parsed
#' from original variable names (e.g. subject1_rotation_width extracts
#' "subject1" as the subject name) and stored in a \code{Subjects} column in the
#' returned tibble.
#'
#' @export
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the july 29 example data included in the package
#' jul_29 <-
#'   read_motive_csv(system.file("extdata", "july-29_group-I_16-20.csv",
#'                              package = 'pathviewR'))
#'
#' ## First use relabel_viewr_axes() to rename these variables using _length,
#' _width, and _height instead
#' jul_29_relabeled <- relabel_viewr_axes(jul_29)
#'
#' ## Now use gather_tunnel_data() to gather colums into tidy format
#' jul_29_gathered <- gather_tunnel_data(jul_29)
#'
#' ## Column names reflect the way in which data were reformatted:
#' names(jul_29_gathered)

gather_tunnel_data <- function(obj_name,
                               NA_drop = TRUE,
                               ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## NOTE: I know that the following 3 blocks of code can be written more
  ## efficiently, but I would rather split them up explicitly so that a user
  ## knows exactly which type of column is missing, given the error message
  ## that is returned.

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Get number of unique subjects
  bodiez <- length(attributes(obj_name)$subject_names_simple)
  if (bodiez == 0) {
    stop("No rigid bodies or markers detected. Please assess your data.")
  }

  ## And the unique subjects' names
  subbiez <- attributes(obj_name)$subject_names_simple

  ## Start setting up a gathered data.frame
  ## Just frame, time, and subject for now
  ## Other columns will be appended
  gathered_data <- data.frame(
    frame = c(rep(obj_name$"frame", bodiez)),
    time_sec = c(rep(obj_name$"time_sec", bodiez))
    )
  ## The data.frame will be dim(obj_name)[1] * bodiez in length
  rb <- NULL
  for (i in 1:bodiez){
    rb <- c(rb,
            rep(subbiez[i], dim(obj_name)[1])
            )
  }
  gathered_data$subject <- rb

  ## Gather positions
    ## Lengths
    tmp_len <-
      obj_name[,grepl("position_length", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$position_length <- tmp_len$value
    ## Widths
    tmp_wid <-
      obj_name[,grepl("position_width", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$position_width <- tmp_wid$value
    ## Heights
    tmp_hei <-
      obj_name[,grepl("position_height", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$position_height <- tmp_hei$value

  ## Gather rotations
    ## Lengths
    tmp_rotl <-
      obj_name[,grepl("rotation_length", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_length <- tmp_rotl$value
    ## Widths
    tmp_rotw <-
      obj_name[,grepl("rotation_width", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_width <- tmp_rotw$value
    ## Heights
    tmp_roth <-
      obj_name[,grepl("rotation_height", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_height <- tmp_roth$value
    ## W
    tmp_rotw <-
      obj_name[,grepl("rotation_real", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_real <- tmp_rotw$value

  ## Gather mean marker error
    tmp_mark <-
      obj_name[,grepl("mean_marker_error", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$mean_marker_error <- tmp_mark$value

  ## Drop NAs if desired
    if (NA_drop == TRUE) {
      gathered_data <- gathered_data %>% tidyr::drop_na()
    } else {
      gathered_data <- gathered_data
    }

  ## Coerce to tibble
  gathered_data <- tibble::as_tibble(gathered_data)

  ## Add metadata as attributes()
  attr(obj_name,"file_id") ->      attr(gathered_data,"file_id")
  attr(obj_name,"file_mtime") ->   attr(gathered_data,"file_mtime")
  attr(obj_name,"header") ->       attr(gathered_data,"header")
  attr(obj_name,"rigid_bodies") -> attr(gathered_data,"rigid_bodies")
  attr(obj_name,"data_names") ->   attr(gathered_data,"data_names")
  attr(obj_name,"d1") ->           attr(gathered_data,"d1")
  attr(obj_name,"d2") ->           attr(gathered_data,"d2")

  ## Leave a note that we reshaped the data
  attr(gathered_data,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "gathered_tunnel")

  ## Export
  return(gathered_data)
}


############################ trim_tunnel_outliers ##############################
## Trim out artifacts and other outliers from the extremes of the tunnel
## User provides estimates of min and max values of data and function then
## trims out anything beyond these estimates.
## I highly recommend plotting data beforehand and checking that estimates
## make sense!!!!!!

trim_tunnel_outliers <- function(obj_name,
                                 lengths_min = 0,
                                 lengths_max = 3,
                                 widths_min = -0.4,
                                 widths_max = 0.8,
                                 heights_min = -0.2,
                                 heights_max = 0.5,
                                 ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that gather_tunnel_data() has been run on the object
  if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
    stop("You must gather your party before venturing forth.
Please use gather_tunnel_data() on this object to gather data columns
into key-value pairs ")
  }

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Filter by heights first, since extremes in this axis tend to be the
  ## noisiest
  filt_heights <-
    obj_name %>%
    dplyr::filter(position_height < heights_max) %>%
    dplyr::filter(position_height > heights_min)

  ## Now filter by lengths
  filt_lengths <-
    filt_heights %>%
    dplyr::filter(position_length < lengths_max) %>%
    dplyr::filter(position_length > lengths_min)

  ## Finally filter by widths; this may not change anything since outliers
  ## in this axis seem to be rare
  filt_widths <-
    filt_lengths %>%
    dplyr::filter(position_width < widths_max) %>%
    dplyr::filter(position_width > widths_min) %>%
    tibble::as_tibble()

  ## Add metadata as attributes()
  attr(obj_name,"file_id") ->      attr(filt_widths,"file_id")
  attr(obj_name,"file_mtime") ->   attr(filt_widths,"file_mtime")
  attr(obj_name,"header") ->       attr(filt_widths,"header")
  attr(obj_name,"rigid_bodies") -> attr(filt_widths,"rigid_bodies")
  attr(obj_name,"data_names") ->   attr(filt_widths,"data_names")
  attr(obj_name,"d1") ->           attr(filt_widths,"d1")
  attr(obj_name,"d2") ->           attr(filt_widths,"d2")

  ## Leave a note that we trimmed tunnel outliers
  attr(filt_widths,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "artifacts_removed")

  ## Export
  return(filt_widths)
}


################################ rotate_tunnel #################################
## Function to rotate a tunnel so that perches are approximately aligned
## Rotation is applied to length and width data; height is untouched
## The user first estimates the locations of the perches by specifying
## bounds for where each perch is located.
## The function then computes the center of each bounding box and estimates
## that to be the midpoint of each perch
## Then the center point of the tunnel (center between the perch midpoints) is
## estimated
## The angle between perch1_center, tunnel_center_point, and arbitrary point
## along the length axis (tunnel_center_point - 1 on length) is estimated.
## That angle is then used to rotate the data, again only in the length and
## width dimensions.
## 2020-02-20 Height is now standardized by (approximate) perch height; values
## greater than 0 are above the perch and values less than 0 are below the
## perch level.

rotate_tunnel <- function(obj_name,
                          all_heights_min = 0.11,
                          all_heights_max = 0.3,
                          ## perch 1 = left (near length = 0); perch 2 = right
                          perch1_len_min = -.06,
                          perch1_len_max = 0.06,
                          perch2_len_min = 2.48,
                          perch2_len_max = 2.6,
                          perch1_wid_min = 0.09,
                          perch1_wid_max = 0.31,
                          perch2_wid_min = 0.13,
                          perch2_wid_max = 0.35,
                          ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that gather_tunnel_data() has been run on the object
  if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
    stop("You must gather your party before venturing forth.
Please use gather_tunnel_data() on this object to gather data columns
into key-value pairs ")
  }

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }


  ## Now compute the approximate midpoints of each perch
  ## Perch 1 first
  perch1_len <- mean(range(perch1_len_min, perch1_len_max))
  perch1_wid <- mean(range(perch1_wid_min, perch1_wid_max))
  perch1_hei <- mean(range(all_heights_min, all_heights_max))
  perch1_midpoint <- c(perch1_len, perch1_wid, perch1_hei)
  ## Now Perch 2
  perch2_len <- mean(range(perch2_len_min, perch2_len_max))
  perch2_wid <- mean(range(perch2_wid_min, perch2_wid_max))
  perch2_hei <- mean(range(all_heights_min, all_heights_max))
  perch2_midpoint <- c(perch2_len, perch2_wid, perch2_hei)

  ## Now approximate the centerpoint of the tunnel
  tunnel_centerpoint <- c(mean(c(perch1_len, perch2_len)), #length
                          mean(c(perch1_wid, perch2_wid)), #width
                          mean(c(perch1_hei, perch2_hei))  #height
                          )

  ## Define an arbitrary point that is in line with the tunnel center on the
  ## width and height axes but an arbitrary distance (say 1 m) away. This will
  ## be necessary for the angular calculation.
  tunnel_arbitrary <- c(tunnel_centerpoint[1] - 1,
                        tunnel_centerpoint[2],
                        tunnel_centerpoint[3])

  ## Now calculate the angle of which the tunnel is offset
  ## Point 1 = perch1_midpoint
  ## Point 2 = tunnel_centerpoint
  ## Point 3 = tunnel_arbitrary
  ## Using xyangles() instead its 3D counterpart because we're gonna keep
  ## height constant throughout
  tunnel_angle <- xyangles(x1 = perch1_len,
                           y1 = perch1_wid,
                           x2 = tunnel_centerpoint[1],
                           y2 = tunnel_centerpoint[2],
                           x3 = tunnel_arbitrary[1],
                           y3 = tunnel_arbitrary[2])

  ## Now convert to radians, which will be used during the rotation later
  alpharad <- deg2rad(tunnel_angle)

  ## Now translate all data in Length and Width so that the tunnel centerpoint
  ## is at (0,0) for (length, width). Keep heights as-is for now?

  ## First the principal points:
  perch1_midpoint_translated <- perch1_midpoint - tunnel_centerpoint
  perch2_midpoint_translated <- perch2_midpoint - tunnel_centerpoint
  tunnel_centerpoint_translated <- tunnel_centerpoint - tunnel_centerpoint
  tunnel_arbitrary_translated <- tunnel_arbitrary - tunnel_centerpoint

  ## Compute the new locations of perch midpoint estimates
  perch1_trans_prime <- c((perch1_midpoint_translated[1]*cos(-1*alpharad))-
                           (perch1_midpoint_translated[2])*sin(-1*alpharad),
                         (perch1_midpoint_translated[1]*sin(-1*alpharad))+
                           (perch1_midpoint_translated[2])*cos(-1*alpharad),
                         perch1_midpoint[3])

  perch2_trans_prime <- c((perch2_midpoint_translated[1]*cos(-1*alpharad))-
                           (perch2_midpoint_translated[2])*sin(-1*alpharad),
                         (perch2_midpoint_translated[1]*sin(-1*alpharad))+
                           (perch2_midpoint_translated[2])*cos(-1*alpharad),
                         perch2_midpoint[3])

  ## Now rotate data in length and width dimensions only
  ## First make a copy of the data. The original data (obj_name) will be used
  ## for the translation step. The new copy (obj_new) will then take that
  ## translated data and rotate it. This is because both length and width
  ## data are simultaneously used during the rotation, so overwriting lengths
  ## will necessarily mess things up for re-defining widths
  obj_name -> obj_new

  ## Now apply a translation to the original data set
  ## (Translation for height in next block of code)
  obj_name$position_length <- obj_name$position_length - tunnel_centerpoint[1]
  obj_name$position_width <- obj_name$position_width - tunnel_centerpoint[2]

  # Now apply a rotation to the translated data set
  obj_new$position_length <- (obj_name$position_length*cos(-1*alpharad))-
    (obj_name$position_width)*sin(-1*alpharad)
  obj_new$position_width <- (obj_name$position_length*sin(-1*alpharad))+
    (obj_name$position_width)*cos(-1*alpharad)
  ## Height will simply be translated
  obj_new$position_height <- obj_name$position_height - tunnel_centerpoint[3]
  ## (all other variables should remain the same)

  ## Coerce to tibble
  obj_new <- tibble::as_tibble(obj_new)

  ## Add new info to attributes that lists the original (approximate) perch
  ## positions, tunnel center point, angle of rotation, and new (approxmate)
  ## perch positions after rotation
  attr(obj_new,"perch1_midpoint_original") <- perch1_midpoint
  attr(obj_new,"perch2_midpoint_original") <- perch2_midpoint
  attr(obj_new,"tunnel_centerpoint_original") <- tunnel_centerpoint
  attr(obj_new,"rotation_degrees") <- tunnel_angle
  attr(obj_new,"rotation_radians") <- alpharad
  attr(obj_new,"perch1_midpoint_current") <- perch1_trans_prime
  attr(obj_new,"perch2_midpoint_current") <- perch2_trans_prime

  ## Leave a note that we rotated and translated the data set
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), c("tunnel_rotated", # rotated
                                          "tunnel_centered") # centered
    )

  ## Export
  return(obj_new)
}


############################# standardize_tunnel ###############################
## Alternative to rotate_tunnel. Writing a version here where perches (or other
## landmarks) are coded as rigid bodies from the get-go.
##
## This may get consolidated with rotate_tunnel() at some point...

standardize_tunnel <- function(obj_name,
                               landmark_one = "perch1",
                               landmark_two = "perch2",
                               ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that gather_tunnel_data() has been run on the object
  if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
    stop("You must gather your party before venturing forth.
Please use gather_tunnel_data() on this object to gather data columns
into key-value pairs ")
  }

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }


  ## ADD CHECK THAT perch1position length < perch2 position_length; otherwise,
  ## the rotation will apply to a mirror-image of the tunnel

  ## Tunnels are standardized via information from perch positions. We think
  ## that the most reasonable esimate for perches' positions are their median
  ## values. So the next few blocks of code will determine the median positions
  ## of each perch and then estimate the centerpoint of the tunnel to be between
  ## the two perches (i.e. mean of each set of perch positions).

  ## Make a data.frame of landmark_one (i.e. perch #1)'s positions
  landmark1_med_pos <- obj_name %>%
    dplyr::filter(subject == landmark_one) %>%
    dplyr::summarise(med_length = median(position_length),
                     med_width = median(position_width),
                     med_height = median(position_height)) %>%
    as.data.frame()

  landmark2_med_pos <- obj_name %>%
    dplyr::filter(subject == landmark_two) %>%
    dplyr::summarise(med_length = median(position_length),
                     med_width = median(position_width),
                     med_height = median(position_height)) %>%
    as.data.frame()

  ## Now approximate the centerpoint of the tunnel
  tunnel_centerpoint <-
    rbind(landmark1_med_pos, landmark2_med_pos) %>% colMeans()

  ## Define an arbitrary point that is in line with the tunnel center on the
  ## width and height axes but an arbitrary distance (say 1 m) away. This will
  ## later be necessary for the angular calculation.
  tunnel_arbitrary <- c(tunnel_centerpoint[1] - 1,
                        tunnel_centerpoint[2],
                        tunnel_centerpoint[3])

  ## Now calculate the angle of which the tunnel is offset
  ## Point 1 = perch1_midpoint
  ## Point 2 = tunnel_centerpoint
  ## Point 3 = tunnel_arbitrary
  ## Using xyangles() instead its 3D counterpart because we're gonna keep
  ## height constant throughout
  tunnel_angle <- xyangles(x1 = as.numeric(landmark1_med_pos)[1],
                           y1 = as.numeric(landmark1_med_pos)[2],
                           x2 = tunnel_centerpoint[1],
                           y2 = tunnel_centerpoint[2],
                           x3 = tunnel_arbitrary[1],
                           y3 = tunnel_arbitrary[2])

  ## Now convert to radians, which will be used during the rotation later
  alpharad <- deg2rad(tunnel_angle)

  ## Now translate all data in Length and Width so that the tunnel centerpoint
  ## is at (0,0) for (length, width). Keep heights as-is for now?

  ## First the principal points:
  perch1_midpoint_translated <- as.numeric(
    as.numeric(landmark1_med_pos) - tunnel_centerpoint)
  perch2_midpoint_translated <- as.numeric(
    as.numeric(landmark2_med_pos) - tunnel_centerpoint)
  tunnel_centerpoint_translated <- tunnel_centerpoint - tunnel_centerpoint
  ## tunnel_centerpoint_translated should be (0, 0, 0)
  tunnel_arbitrary_translated <- tunnel_arbitrary - tunnel_centerpoint
  ## tunnel_abitrary_translated should be (-1, 0, 0)

  ## Compute the new locations of perch midpoint estimates
  perch1_trans_prime <- c((perch1_midpoint_translated[1]*cos(-1*alpharad))-
                            (perch1_midpoint_translated[2])*sin(-1*alpharad),
                          (perch1_midpoint_translated[1]*sin(-1*alpharad))+
                            (perch1_midpoint_translated[2])*cos(-1*alpharad),
                          as.numeric(landmark1_med_pos)[3])

  perch2_trans_prime <- c((perch2_midpoint_translated[1]*cos(-1*alpharad))-
                            (perch2_midpoint_translated[2])*sin(-1*alpharad),
                          (perch2_midpoint_translated[1]*sin(-1*alpharad))+
                            (perch2_midpoint_translated[2])*cos(-1*alpharad),
                          as.numeric(landmark2_med_pos)[3])

  ## Now rotate data in length and width dimensions only
  ## First make a copy of the data. The original data (obj_name) will be used
  ## for the translation step. The new copy (obj_new) will then take that
  ## translated data and rotate it. This is because both length and width
  ## data are simultaneously used during the rotation, so overwriting lengths
  ## will necessarily mess things up for re-defining widths
  obj_name -> obj_new

  ## Now apply a translation to the original data set
  ## (Translation for height in next block of code)
  obj_name$position_length <- obj_name$position_length - tunnel_centerpoint[1]
  obj_name$position_width <- obj_name$position_width - tunnel_centerpoint[2]

  # Now apply a rotation to the translated data set
  obj_new$position_length <- (obj_name$position_length*cos(-1*alpharad))-
    (obj_name$position_width)*sin(-1*alpharad)
  obj_new$position_width <- (obj_name$position_length*sin(-1*alpharad))+
    (obj_name$position_width)*cos(-1*alpharad)
  ## Height will simply be translated
  obj_new$position_height <- obj_name$position_height - tunnel_centerpoint[3]
  ## (all other variables should remain the same)

  ## Coerce to tibble
  obj_new <- tibble::as_tibble(obj_new)

  ## Add new info to attributes that lists the original (approximate) perch
  ## positions, tunnel center point, angle of rotation, and new (approxmate)
  ## perch positions after rotation
  attr(obj_new,"landmark1_midpoint_original") <- as.numeric(landmark1_med_pos)
  attr(obj_new,"landmark2_midpoint_original") <- as.numeric(landmark2_med_pos)
  attr(obj_new,"tunnel_centerpoint_original") <- tunnel_centerpoint
  attr(obj_new,"rotation_degrees") <- tunnel_angle
  attr(obj_new,"rotation_radians") <- alpharad
  attr(obj_new,"landmark1_midpoint_current") <- perch1_trans_prime
  attr(obj_new,"landmark2_midpoint_current") <- perch2_trans_prime

  ## Leave a note that we rotated and translated the data set
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), c("tunnel_rotated", # rotated
                                          "tunnel_centered") # centered
    )

  ## Export
  return(obj_new)
}


############################### select_x_percent ###############################
## Select data in the middle X percent of the length of the tunnel

select_x_percent <- function(obj_name,
                             desired_percent = 33,
                             ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that it's undergone one of our centering steps
  if (!any(attr(obj_name,"pathviewR_steps") == "tunnel_centered")) {
    warning("This viewr object does not seem to have been passed through
one of our centering options, e.g. rotate_tunnel(), standardize_tunnel(),
or center_tunnel(). Please proceed with extreme caution.")
  }

  ## Convert percent to proportion
  prop <- desired_percent/100

  ## Get tunnel length
  tunnel_range <- range(obj_name$position_length)
  tunnel_length <- sum(abs(tunnel_range[1]), abs(tunnel_range[2]))

  ## Determine the range of lengths that will be needed
  ## Multiply tunnel length by the proportion, then divide by 2 to get
  ## the postive (towards the right) and negative (towards the left) halves
  lengths_needed <- (tunnel_length * prop)/2

  ## Now filter by lengths
  obj_name <-
    obj_name %>%
    dplyr::filter(position_length < lengths_needed) %>%
    dplyr::filter(position_length > (-1 * lengths_needed))

  ## Coerce to tibble
  obj_name <- tibble::as_tibble(obj_name)

  ## Leave a note about the proportion used
  attr(obj_name,"percent_selected") <- desired_percent
  attr(obj_name,"full_tunnel_length") <- tunnel_length
  attr(obj_name,"selected_tunnel_length") <- tunnel_length * prop

  ## Leave a note that we rotated and translated the data set
  attr(obj_name,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "percent_selected")

  ## Export
  return(obj_name)
}


############################# separate_trajectories ############################
## Specify a maximum frame gap and then use this to separate rows of data
## into separately labeled trajectories.

separate_trajectories <- function(obj_name,
                                  max_frame_gap = 1,
                                  ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  obj_name$frame -> framez
  df <-
    framez %>%
    tibble::tibble(dat = .) # automatically gives name "." for frames so rename
    # this to dat
  sploot <-
    df %>%
    dplyr::group_by(seq_id = cumsum(c(1, diff(dat)) > max_frame_gap))# group by
  # seq_id which is the diff between successive frames is greater than gap
  obj_name$traj_id <- sploot$seq_id # new column (traj_id) is this seq_id

  ## Also combine the rigid body ID so that we're sure trajectories correspond
  ## to unique rigid bodies
  obj_name$sub_traj <- paste0(obj_name$subject,"_",obj_name$traj_id)

  ## Coerce to tibble
  obj_name <- tibble::as_tibble(obj_name)

  ## Leave a note about the max frame gap used
  attr(obj_name,"max_frame_gap") <- max_frame_gap

  ## Leave a note that we rotated and translated the data set
  attr(obj_name,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "trajectories_labeled")

  ## Export
  return(obj_name)

}


############################# get_full_trajectories ############################
## Specify a minimum span of the (selected) tunnel and then keep trajectories
## that are wider than that span and go from one end to the other

get_full_trajectories <- function(obj_name,
                                  span = 0.8,
                                  ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that its axes have been renamed
  if (!any(attr(obj_name,"pathviewR_steps") == "trajectories_labeled")) {
    stop("Please use separate_trajectories() prior to using this")
  }

  summary_obj <-
    obj_name %>%
    dplyr::group_by(traj_id) %>%
    dplyr::summarise(traj_length = n(),
                     start_length = position_length[1],
                     end_length = position_length[traj_length],
                     length_diff = abs(end_length - start_length),
                     start_length_sign = sign(start_length),
                     end_length_sign = sign(end_length))

  ## Define rightwards as starting at negative length values and going
  ## towards positive
  summary_obj$direction <-
    ifelse(summary_obj$start_length < 0, "rightwards", "leftwards")

  ## If selected lengths have been stripped away, compute the maximum
  ## length again
  if (is.null(attr(obj_name, "selected_tunnel_length"))) {
    max_length <- sum(abs(range(obj_name$position_length)[1]),
                      abs(range(obj_name$position_length)[2]))
  } else {
    # Otherwise, use the selected_tunnel_length
    max_length <- attr(obj_name,"selected_tunnel_length")
  }

  ## Filter data by the two criteria
  filt_summary <-
    summary_obj %>%
    dplyr::group_by(traj_id) %>%
    ## Each trajectory must span a minimum porportion of the selected tunnel
    dplyr::filter(length_diff > (span * max_length)) %>%
    ## And the signs (+ or -) at the ends of the trajectories must be opposites
    dplyr::filter(start_length_sign != end_length_sign)

  obj_continuous <-
    obj_name %>%
    dplyr::filter(traj_id %in% filt_summary$traj_id)

  ## Join the columns to add in direction
  obj_defined <-
    dplyr::right_join(obj_continuous, filt_summary, by = "traj_id") %>%
    tibble::as_tibble()

  ## Leave a note about the span used
  attr(obj_defined, "span") <- span

  ## Leave a note that full trajectories were retained
  attr(obj_defined,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "full_trajectories")

  ## Export
  return(obj_defined)

}


############################### remove birds with too few flights ###############################
rmbird_byflightnum <- function(obj_name,
                               flightnum = 5,
                               ...){
  #get list of birds that complete x num flights in BOTH treatments
  rm_bytreat <-
    obj_name %>%
    unite(block, "bird", "treatment", sep = "_") %>%
    group_by(block) %>%
    nest() %>%
    mutate(n = data %>%
             map_dbl(~ length(.$traj))) %>%
    select(block, n) %>%
    separate(block, c("bird", "treatment")) %>%
    spread(treatment, n)

  #not fully functional cause can't figure out how to make the column a changeable argument
  rm_bytreat <-
    rm_bytreat %>%
    filter(up >= flightnum & down >= flightnum) %>%
    select(bird)

  obj_name <- inner_join(obj_name, rm_bytreat)

}


############################### plot by bird ###############################
## Generate plots of each individual--hoping to loop to auto go through all birds in each treatment...

plot_by_bird_manually <- function(obj_name){

  paths_obj_name <- ggplot(obj_name) +
    geom_point(aes(position_length, position_width, colour = treatment), alpha = .1, show.legend = FALSE) +
    theme(legend.position = "none") +
    theme_tufte()

  hist_obj_name <- ggplot(obj_name) +
    geom_histogram(aes(position_width, fill = treatment), alpha = .5, position = "identity", show.legend = FALSE) +
    coord_flip() +
    theme(legend.position = "none") +
    theme_tufte()

  obj_name_plot <- grid.arrange(paths_obj_name,hist_obj_name, nrow=1, widths=c(2,.5))

  ggsave(paste(deparse(substitute(obj_name)), ".png", sep = ""), obj_name_plot,
         path = "C:/Users/Melis/Documents/GoogleDrive/Altshuler/thesis/ZFVG/R plots/bybird")

  length(unique(obj_name$traj_id))
}

## to use purr, need to rearrange
## not sure at this time how to use this to combine both plots (paths and hist) so have them separate for now

purrplot_by_bird <- function(obj_name,
                             treatment,
                             ...){
  #set axes limits based on data
  height_limits <- c(max(abs(range(obj_name$position_height)))*-1,
                     max(abs(range(obj_name$position_height))))
  width_limits <- c(max(abs(range(obj_name$position_width)))*-1,
                    max(abs(range(obj_name$position_width))))

  #for top view (change in lateral position)
  top_view <- obj_name %>%
    group_by(rigid_body) %>%
    nest() %>%
    mutate(paths = map(data, ~ggplot(., aes(position_length, position_width, colour = treatment)) +
                         geom_point(alpha = .1, show.legend = FALSE) +
                         ylim(width_limits) +
                         geom_hline(yintercept = 0, linetype = "dotted") +
                         coord_fixed(ratio = 1) +
                         theme_tufte()),
           hist = map(data, ~ggplot(., aes(x = position_width, fill = treatment)) +
                        geom_density(alpha = .5, position = "identity", show.legend = FALSE) +
                        xlim(width_limits) +
                        geom_vline(xintercept = 0, linetype = "dotted") +
                        coord_flip() +
                        theme_tufte()),
           filename = paste0(rigid_body,"_top",".png"))

  #all of them together:
  top_all_plots <- top_view %>%
    select(rigid_body, paths, hist) %>%
    gather("plot_type", "allplots", 2:3)

  birdseye_view <- plot_grid(plotlist = top_all_plots[[3]])

  ggsave(paste0(treatment,"_", "topview.png"), birdseye_view,
         path = "C:/Users/Melis/Documents/GoogleDrive/Altshuler/thesis/ZFVG/R plots/bybird")

  #for elev view (change in height):
  elev_view <- obj_name %>%
    group_by(rigid_body) %>%
    nest() %>%
    mutate(paths = map(data, ~ggplot(., aes(position_length, position_height, colour = treatment)) +
                         geom_point(alpha = .1, show.legend = FALSE) +
                         ylim(height_limits) +
                         geom_hline(yintercept = 0, linetype = "dotted") +
                         coord_fixed(ratio = 1) +
                         theme_tufte()),
           hist = map(data, ~ggplot(., aes(position_height, fill = treatment)) +
                        geom_density(alpha = .5, position = "identity", show.legend = FALSE) +
                        xlim(height_limits) +
                        geom_vline(xintercept = 0, linetype = "dotted") +
                        coord_flip() +
                        theme_tufte()),
           filename = paste0(rigid_body,"_elev",".png"))

  #all of them together:
  elev_all_plots <- elev_view %>%
    select(rigid_body, paths, hist) %>%
    gather("plot_type", "allplots", 2:3)

  side_view <- plot_grid(plotlist = elev_all_plots[[3]])

  ggsave(paste0(treatment,"_", "elevview.png"), side_view,
         path = "C:/Users/Melis/Documents/GoogleDrive/Altshuler/thesis/ZFVG/R plots/bybird")
}

#to save each plot separately:
#AB_top %>%
#  select(filename, paths) %>%
#  pwalk(ggsave, path = "C:/Users/Melis/Documents/GoogleDrive/Altshuler/thesis/ZFVG/R plots/bybird")


############################### select_X_percent ###############################
## Select data in the middle X percent of the length of the tunnel

select_x_percent_M <- function(obj_name,
                               desired_percent = 33,
                               ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  # ## Check that its axes have been renamed
  # if (!any(class(obj_name) == "tunnel_rotated")) {
  #   stop("Please use rotate_tunnel() to align data prior to using this")
  # }

  ## Convert percent to proportion
  prop <- desired_percent/100

  ## Get tunnel length
  tunnel_range <- range(obj_name$position_length)
  tunnel_length <- sum(abs(tunnel_range[1]), abs(tunnel_range[2]))

  ## Determine the range of lengths that will be needed
  ## Multiply tunnel length by the proportion, then divide by 2 to get
  ## the postive (towards the right) and negative (towards the left) halves
  lengths_needed <- (tunnel_length * prop)/2
  midpoint <- tunnel_length / 2

  ## Now filter by lengths
  obj_name <-
    obj_name %>%
    dplyr::filter(position_length < midpoint + lengths_needed) %>%
    dplyr::filter(position_length > (midpoint - lengths_needed)) %>%
    tibble::as_tibble()

  ## Leave a note about the proportion used
  attr(obj_name,"percent_selected") <- desired_percent
  attr(obj_name,"full_tunnel_length") <- tunnel_length
  attr(obj_name,"selected_tunnel_length") <- tunnel_length * prop

  ## Leave a note that we rotated and translated the data set
  attr(obj_name,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "percent_selected_M")

  ## Export
  return(obj_name)
}


############################# determine_frame_gap ############################
## run separate_trajectories with many different frame gaps to help determine what value to use
## spits out table and plot to guide decision--maybe could be incorporated into separate_trajectories
## in future to guess at a default value

determine_fg_M <- function(obj_name,
                           loops = 20,
                           ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  # make a bunch of empty vectors to dump info
  mfg <- vector("list",loops)
  cts <- vector("list",loops)
  nrows <- vector(mode = "double",loops)
  mfg_id <- vector(mode = "double", loops)

  # loop through user defined number of max frame gap values
  i <- 1
  while (i < loops + 1) {
    mfg[[i]] = separate_trajectories(obj_name, max_frame_gap = i)
    cts[[i]] = count(mfg[[i]], traj_id)
    nrows[i] = nrow(cts[[i]])
    mfg_id[i] = i
    i = i +1
  }

  #generate user friendly table and plot
  mfg_tib <- tibble::tibble(nrows,mfg_id,file_id = "obj_name")
  mfg_plot <- plot(mfg_tib$mfg_id, mfg_tib$nrows)

  #ideally would then auto-select frame gap based on analysis of curve/plateau?
  #or maybe this should always be looked at by user and then each decision added to lookup table as well?
  return(list(mfg_tib, mfg_plot))

}
