## Last updated: 2020-04-03 VBB

############################### relabel_motiv_axes #############################
## My intuition is that X should be tunnel width, Y should be tunnel length
## and Z should be tunnel height. But the axes may not have been defined as such
## during the recording.
## 
## This function takes a `motiv` object and allows the user to rename the 
## variables.
## 
## Each argument must have a leading underscore ("_") and each argument must 
## have an entry. E.g. tunnel_length = "_Y" will replace all instances of _Y 
## with _length in the names of variables.

relabel_motiv_axes <- function(obj_name,
                               tunnel_length = "_Z",
                               tunnel_width = "_X",
                               tunnel_height = "_Y",
                               ...){
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
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
  ## sub() doesn't seem to be pipe-friendly, so we'll do it this way...
  namez <- sub(tunnel_width,"_width",namez)
  namez <- sub(tunnel_length,"_length",namez)
  namez <- sub(tunnel_height,"_height",namez)
    
  ## Assign names 
  namez -> names(obj_name)
  
  ## Leave a note that the axes have been renamed
  class(obj_name) <- c(class(obj_name), "renamed_tunnel")
  
  ## Export
  return(obj_name)
}


############################# gather_tunnel_data ###############################
## Gather data columns

gather_tunnel_data <- function(obj_name,
                               NA_drop = TRUE,
                               ...){
  
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }
  
  ## Check that its axes have been renamed
  if (!any(class(obj_name) == "renamed_tunnel")) {
    stop("Please rename axes via relabel_motiv_axes() prior to using this")
  } 
  
  ## Get number of rigid bodies
  bodiez <- length(attributes(obj_name)$rigid_bodies)
  if (bodiez == 0) {
    stop("No rigid bodies detected. Please assess your data.")
  }
  
  ## And the rigid body names
  rbeez <- attributes(obj_name)$rigid_bodies
  
  ## Start setting up a gathered data.frame
  ## Just Frame, Time, and Rigid Bodies for now
  ## Other columns will be appended
  gathered_data <- data.frame(
    Frame = c(rep(obj_name$"Frame", bodiez)),
    Time_sec = c(rep(obj_name$"Time (Seconds)", bodiez))
    )
  ## The data.frame will be dim(obj_name)[1] * bodiez in length
  rb <- NULL
  for (i in 1:bodiez){
    rb <- c(rb, 
            rep(rbeez[i], dim(obj_name)[1])
            )
  }
  gathered_data$rigid_body <- rb
  
  ## Gather positions
    ## Lengths
    tmp_len <-
      obj_name %>%
      dplyr::select(tidyselect::contains("Position_length", 
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Position_lengths <- tmp_len$value
    ## Widths
    tmp_wid <-
      obj_name %>%
      dplyr::select(tidyselect::contains("Position_width", 
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Position_widths <- tmp_wid$value
    ## Heights
    tmp_hei <-
      obj_name %>%
      dplyr::select(tidyselect::contains("Position_height", 
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Position_heights <- tmp_hei$value
  
  ## Gather rotations
    ## Lengths
    tmp_rotl <- 
      obj_name %>%
      dplyr::select(tidyselect::contains("Rotation_length",
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Rotation_lengths <- tmp_rotl$value
    ## Widths
    tmp_rotw <-
      obj_name %>%
      dplyr::select(tidyselect::contains("Rotation_width", 
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Rotation_widths <- tmp_rotw$value
    ## Heights
    tmp_roth <-
      obj_name %>%
      dplyr::select(tidyselect::contains("Rotation_height", 
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Rotation_heights <- tmp_roth$value
    ## W
    tmp_rotw <-
      obj_name %>%
      dplyr::select(tidyselect::contains("Rotation_W", 
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Rotation_Ws <- tmp_rotw$value
    
  ## Gather Mean Marker Error
    tmp_mark <-
      obj_name %>%
      dplyr::select(tidyselect::contains("Mean Marker Error", 
                                         ignore.case = FALSE)) %>%
      tidyr::gather()
    gathered_data$Mean_Marker_Error <- tmp_mark$value
    
  ## Drop NAs if desired
    if (NA_drop == TRUE) {
      gathered_data <- gathered_data %>% tidyr::drop_na()
    } else {
      gathered_data <- gathered_data
    }
    
  ## Add metadata as attributes()
  attr(obj_name,"file_id") ->      attr(gathered_data,"file_id")
  attr(obj_name,"file_mtime") ->   attr(gathered_data,"file_mtime")
  attr(obj_name,"header") ->       attr(gathered_data,"header")
  attr(obj_name,"rigid_bodies") -> attr(gathered_data,"rigid_bodies")
  attr(obj_name,"data_names") ->   attr(gathered_data,"data_names")
  attr(obj_name,"d1") ->           attr(gathered_data,"d1")
  attr(obj_name,"d2") ->           attr(gathered_data,"d2")
    
  ## Leave a note that we reshaped the data
  class(gathered_data) <- c(class(obj_name), "gathered_tunnel")
    
  ## Export
  return(gathered_data) 
}


############################ trim_tunnel_outliers ##############################
## Trim out artifacts and other outliers from the extremes of the tunnel
## User provides estimates of min and max values of data and function then
## trims out anything beyond these estimates.
## I highly recommend plotting data beforehand and checking that estimates
## make sense!!!!!!

## NOTE TBD: make an option in the function to let data pass if there are
## no outliers

trim_tunnel_outliers <- function(obj_name,
                                 lengths_min = 0,
                                 lengths_max = 3,
                                 widths_min = -0.4,
                                 widths_max = 0.8,
                                 heights_min = -0.2,
                                 heights_max = 0.5,
                                 ...){
  
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }
  
  ## Check that its axes have been renamed
  if (!any(class(obj_name) == "renamed_tunnel")) {
    stop("Please rename axes via relabel_motiv_axes() prior to using this")
  }
  
  ## Check that the data columns have been gathered
  if (!any(class(obj_name) == "gathered_tunnel")) {
    stop("Please use gather_tunnel_data() to gather columns first")
  }

  ## Filter by heights first, since extremes in this axis tend to be the
  ## noisiest
  filt_heights <- 
    obj_name %>% 
    dplyr::filter(Position_heights < heights_max) %>%
    dplyr::filter(Position_heights > heights_min) 
  
  ## Now filter by lengths 
  filt_lengths <- 
    filt_heights %>% 
    dplyr::filter(Position_lengths < lengths_max) %>%
    dplyr::filter(Position_lengths > lengths_min) 
  
  ## Finally filter by widths; this may not change anything since outliers
  ## in this axis seem to be rare 
  filt_widths <- 
    filt_lengths %>% 
    dplyr::filter(Position_widths < widths_max) %>%
    dplyr::filter(Position_widths > widths_min) 
  
  ## Add metadata as attributes()
  attr(obj_name,"file_id") ->      attr(filt_widths,"file_id")
  attr(obj_name,"file_mtime") ->   attr(filt_widths,"file_mtime")
  attr(obj_name,"header") ->       attr(filt_widths,"header")
  attr(obj_name,"rigid_bodies") -> attr(filt_widths,"rigid_bodies")
  attr(obj_name,"data_names") ->   attr(filt_widths,"data_names")
  attr(obj_name,"d1") ->           attr(filt_widths,"d1")
  attr(obj_name,"d2") ->           attr(filt_widths,"d2")
  
  
  ## Leave a note that we trimmed tunnel outliers
  class(filt_widths) <- c(class(obj_name), "artifacts_removed")
  
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
  
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }
  
  ## Check that its axes have been renamed
  if (!any(class(obj_name) == "artifacts_removed")) {
    stop("Please use trim_tunnel_outliers() prior to using this")
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
  obj_name$Position_lengths <- obj_name$Position_lengths - tunnel_centerpoint[1]
  obj_name$Position_widths <- obj_name$Position_widths - tunnel_centerpoint[2]
  
  # Now apply a rotation to the translated data set
  obj_new$Position_lengths <- (obj_name$Position_lengths*cos(-1*alpharad))-
    (obj_name$Position_widths)*sin(-1*alpharad)
  obj_new$Position_widths <- (obj_name$Position_lengths*sin(-1*alpharad))+
    (obj_name$Position_widths)*cos(-1*alpharad)
  ## Height will simply be translated
  obj_new$Position_heights <- obj_name$Position_heights - tunnel_centerpoint[3]
  ## (all other variables should remain the same)

  ## Add new info to attributes that lists the original (approximate) perch
  ## positions, tunnel center point, angle of rotation, and new (approxmate)
  ## perch positions after rotation
  attr(obj_new,"perch1_midpoint_original") <- perch1_midpoint
  attr(obj_new,"perch2_midpoint_original") <- perch2_midpoint
  attr(obj_new,"tunnel_centerpoint_original") <- tunnel_centerpoint
  attr(obj_new,"rotation_degrees") <- tunnel_angle
  attr(obj_new,"rotation_radians") <- alpharad
  attr(obj_new,"perch1_current_midpoint") <- perch1_trans_prime
  attr(obj_new,"perch2_current_midpoint") <- perch2_trans_prime
  
  ## Leave a note that we rotated and translated the data set
  class(obj_new) <- c(class(obj_name), "tunnel_rotated")
  
  ## Export
  return(obj_new)
}


############################# standardize_tunnel ###############################
## Alternative to rotate_tunnel. Writing a version here where perches (or other
## landmarks) are coded as rigid bodies from the get-go.
## 
## I anticipate that the run order will actually now go:
## read -> relabel -> standardize -> gather -> trim -> selectX -> etcetc

standardize_tunnel <- function(obj_name,
                               landmark_one = "perch1",
                               landmark_two = "perch2",
                               ...){
  
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }

  landmark1_med_pos <- obj_name %>% 
    dplyr::filter(rigid_body == landmark_one) %>% # you mean dowel01?
    dplyr::summarise(med_length = median(Position_lengths),
                     med_width = median(Position_widths),
                     med_height = median(Position_heights)) %>%
    as.data.frame() # make df of just median values of perches
  
  landmark2_med_pos <- obj_name %>% 
    dplyr::filter(rigid_body == landmark_two) %>%
    dplyr::summarise(med_length = median(Position_lengths),
                     med_width = median(Position_widths),
                     med_height = median(Position_heights)) %>%
    as.data.frame()
  
  ## Now approximate the centerpoint of the tunnel
  tunnel_centerpoint <- 
    rbind(landmark1_med_pos, landmark2_med_pos) %>% colMeans()
  # binding dfs by binding rows not columns (cbind)
  
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
  tunnel_arbitrary_translated <- tunnel_arbitrary - tunnel_centerpoint
  
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
  obj_name$Position_lengths <- obj_name$Position_lengths - tunnel_centerpoint[1]
  obj_name$Position_widths <- obj_name$Position_widths - tunnel_centerpoint[2]
  
  # Now apply a rotation to the translated data set
  obj_new$Position_lengths <- (obj_name$Position_lengths*cos(-1*alpharad))-
    (obj_name$Position_widths)*sin(-1*alpharad)
  obj_new$Position_widths <- (obj_name$Position_lengths*sin(-1*alpharad))+
    (obj_name$Position_widths)*cos(-1*alpharad)
  ## Height will simply be translated
  obj_new$Position_heights <- obj_name$Position_heights - tunnel_centerpoint[3]
  ## (all other variables should remain the same)
  
  ## Add new info to attributes that lists the original (approximate) perch
  ## positions, tunnel center point, angle of rotation, and new (approxmate)
  ## perch positions after rotation
  attr(obj_new,"perch1_midpoint_original") <- as.numeric(landmark1_med_pos)
  attr(obj_new,"perch2_midpoint_original") <- as.numeric(landmark2_med_pos)
  attr(obj_new,"tunnel_centerpoint_original") <- tunnel_centerpoint
  attr(obj_new,"rotation_degrees") <- tunnel_angle
  attr(obj_new,"rotation_radians") <- alpharad
  attr(obj_new,"perch1_current_midpoint") <- perch1_trans_prime
  attr(obj_new,"perch2_current_midpoint") <- perch2_trans_prime
  
  ## Leave a note that we rotated and translated the data set
  class(obj_new) <- c(class(obj_name), "tunnel_rotated")
  
  ## Export
  return(obj_new)
}


############################### select_X_percent ###############################
## Select data in the middle X percent of the length of the tunnel

select_x_percent <- function(obj_name,
                             desired_percent = 33,
                             ...){
  
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }
  
  # ## Check that its axes have been renamed
  # if (!any(class(obj_name) == "tunnel_rotated")) {
  #   stop("Please use rotate_tunnel() to align data prior to using this")
  # } 
  
  ## Convert percent to proportion
  prop <- desired_percent/100
  
  ## Get tunnel length
  tunnel_range <- range(obj_name$Position_lengths)
  tunnel_length <- sum(abs(tunnel_range[1]), abs(tunnel_range[2]))
  
  ## Determine the range of lengths that will be needed
  ## Multiply tunnel length by the proportion, then divide by 2 to get 
  ## the postive (towards the right) and negative (towards the left) halves
  lengths_needed <- (tunnel_length * prop)/2
  
  ## Now filter by lengths 
  obj_name <- 
    obj_name %>% 
    dplyr::filter(Position_lengths < lengths_needed) %>%
    dplyr::filter(Position_lengths > (-1 * lengths_needed)) 
  
  ## Leave a note about the proportion used
  attr(obj_name,"percent_selected") <- desired_percent
  attr(obj_name,"full_tunnel_length") <- tunnel_length
  attr(obj_name,"selected_tunnel_length") <- tunnel_length * prop 
  
  ## Leave a note that we rotated and translated the data set
  class(obj_name) <- c(class(obj_name), "percent_selected")
  
  ## Export
  return(obj_name)
} 


############################# separate_trajectories ############################
## Specify a maximum frame gap and then use this to separate rows of data
## into separately labeled trajectories.

separate_trajectories <- function(obj_name, 
                                  max_frame_gap = 1,
                                  ...){
  
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }
  
  # ## Check that its axes have been renamed
  if (!any(class(obj_name) == "tunnel_rotated")) {
    stop("Please use rotate_tunnel() to align data prior to using this")
  } 
  
  obj_name$Frame -> framez
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
  obj_name$rb_traj <- paste0(obj_name$rigid_body,"_",obj_name$traj_id)
  
  ## Leave a note about the max frame gap used
  attr(obj_name,"max_frame_gap") <- max_frame_gap
  
  ## Leave a note that we rotated and translated the data set
  class(obj_name) <- c(class(obj_name), "trajectories_labeled")
  
  ## Export
  return(obj_name)
  
}  


############################# get_full_trajectories ############################
## Specify a minimum span of the (selected) tunnel and then keep trajectories 
## that are wider than that span and go from one end to the other

get_full_trajectories <- function(obj_name, 
                                  span = 0.8,
                                  ...){
  
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }
  
  ## Check that its axes have been renamed
  if (!any(class(obj_name) == "trajectories_labeled")) {
    stop("Please use separate_trajectories() prior to using this")
  } 
  
  summary_obj <- 
    obj_name %>% 
    dplyr::group_by(traj_id) %>% 
    dplyr::summarise(traj_length = n(),
                     start_length = Position_lengths[1],
                     end_length = Position_lengths[traj_length],
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
    max_length <- sum(abs(range(obj_name$Position_lengths)[1]), 
                      abs(range(obj_name$Position_lengths)[2]))
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
    dplyr::right_join(obj_continuous, filt_summary, by = "traj_id")
  
  ## Leave a note about the span used
  attr(obj_defined, "span") <- span
  
  ## Leave a note that full trajectories were retained
  class(obj_defined) <- c(class(obj_name), "full_trajectories")
  
  ## Export
  return(obj_defined)
  
}


############################### all_in_one_function ############################
## Use all of the preceding functions to construct an all-in-one function for 
## ease of use. Unsure if this is the best way to go, but let's give it a try
## anyway.

import_and_clean_motiv <- function(file_name,
                                  file_id = NA,
                                  ...){
  
  ## Import checks
  if (missing(file_name)) 
    stop("A file_name is required")
  if (!file.exists(file_name)) 
    stop(paste0("File ", file_name, " not found!"))
  
  ## Check that any arguments supplied are valid; return a warning if not
  valid_args <- c(
    ## read_motiv_csv()
    "file_name", "file_id", 
    ## relabel_motiv_axes()
    "tunnel_length", "tunnel_width", "tunnel_height",
    ## trim_tunnel_outliers()
    "lengths_min", "lengths_max", 
    "widths_min", "widths_max", 
    "heights_min", "heights_max",
    ## rotate_tunnel()
    "all_heights_min", "all_heights_max", 
      ## perch 1 = left (near length = 0); perch 2 = right
    "perch1_len_min", "perch1_len_max",
    "perch2_len_min", "perch2_len_max", 
    "perch1_wid_min", "perch1_wid_max", 
    "perch2_wid_min", "perch2_wid_max",
    ## select_x_percent()
    "desired_percent",
    ## separate_trajectories()
    "max_frame_gap", 
    ## get_full_trajectories()
    "span"
  )
  arg_names <- names(list(...))
  if (!all(arg_names %in% valid_args)) {
    warning("One or more provided arguments does not match known arguments.
            \nThese will not be used.")
  }
  
  ## Run it through the pipe
  obj <-
    file_name %>%
    read_motiv_csv(...) %>%
    relabel_motiv_axes(...) %>%
    gather_tunnel_data(...) %>%
    trim_tunnel_outliers(...) %>%
    rotate_tunnel(...) %>%
    select_x_percent(...) %>%
    separate_trajectories(...) %>%
    get_full_trajectories(...)
    
  ## Export
  return(obj)
}