## work space for addressing visual perception functions reviews

## change argument for vertex_angle to mean the actual angle of the V in the
## V-shaped tunnel. Build into insert_treatments() a calculation that makes it
## work with calc_min_dist functions

## insert_treatments()
## use tunnel_length argument so that front/back wall distances can be calculated

insert_treatments <- function(obj_name,
                              perch_2_vertex = NULL,
                              vertex_angle = NULL, # actual angle of vertex
                              tunnel_width = NULL,
                              tunnel_length = NULL,
                              stim_param_lat_pos = NULL,
                              stim_param_lat_neg = NULL,
                              stim_param_end_pos = NULL,
                              stim_param_end_neg = NULL,
                              treatment = NULL){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_full_trajectories has been run prior to use
  if (!any(attr(obj_name, "pathviewR_steps") == "full_trajectories")){
    stop("Run get_full_trajectories() prior to use")
  }


  ## Translate arguments into variables at beginning of data frame
  ##
  ## figure out how to do it such that only the arguments supplied are added to
  ## the data frame
  if (attr(obj_name, "import_method") == "motive"){
    obj_name <- tibble::add_column(obj_name, .before = "frame",
                                   perch_2_vertex = perch_2_vertex,
                                   vertex_angle = deg_2_rad(vertex_angle/2),
                                   tunnel_width = tunnel_width,
                                   tunnel_length = tunnel_length,
                                   stim_param_lat_pos = stim_param_lat_pos,
                                   stim_param_lat_neg = stim_param_lat_neg,
                                   stim_param_end_pos = stim_param_end_pos,
                                   stim_param_end_neg = stim_param_end_neg,
                                   treatment = treatment)
  } else if (attr(obj_name, "import_method") == "flydra"){
    obj_name <- tibble::add_column(obj_name, .before = "frame",
                                   tunnel_width = tunel_width,
                                   tunnel_length= tunnel_length,
                                   stim_param_lat_pos = stim_param_lat_pos,
                                   stim_param_lat_neg = stim_param_lat_neg,
                                   stim_param_end_pos = stim_param_end_pos,
                                   stim_param_end_neg = stim_param_end_neg,
                                   treatment = treatment)
  }

  ## Add arguments into metadata......surewhynot
  if (attr(obj_name, "import_method") == "motive"){
    attr(obj_name, "perch_2_vertex") <- perch_2_vertex
    attr(obj_name, "vertex_angle") <- vertex_angle
    attr(obj_name, "tunnel_width") <- tunnel_width
    attr(obj_name, "tunnel_length") <- tunnel_length
    attr(obj_name, "stim_param_lat_pos") <- stim_param_lat_pos
    attr(obj_name, "stim_param_lat_neg") <- stim_param_lat_neg
    attr(obj_name, "stim_param_end_pos") <- stim_param_end_pos
    attr(obj_name, "stim_param_end_neg") <- stim_param_end_neg
    attr(obj_name, "treatment") <- treatment
  } else if (attr(obj_name, "import_method") == "flydra"){
    attr(obj_name, "tunnel_width") <- tunnel_width
    attr(obj_name, "tunnel_length") <- tunnel_length
    attr(obj_name, "stim_param_lat_pos") <- stim_param_lat_pos
    attr(obj_name, "stim_param_lat_neg") <- stim_param_lat_neg
    attr(obj_name, "stim_param_end_pos") <- stim_param_end_pos
    attr(obj_name, "stim_param_end_neg") <- stim_param_end_neg
    attr(obj_name, "treatment") <- treatment
  }

  ## Leave note that treatments were added
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "treatments_added")
  return(obj_name)
}



# min_dist function for V-shaped tunnel
# get min_dist for front/back wall by using direction of bird flight to
# determine the distance to the opposite wall. Then can use this distance to
# get vis_angle, sf, tf, image vel, and image expansion from the front of the
# tunnel
calc_min_dist_v <- function(obj_name,
                           simplify = TRUE){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "treatments_added")){
    stop("Please run insert_treatments() prior to use")
  }

  ## duplicate object for simplify = TRUE
  obj_simplify <- obj_name

          ## For distance to lateral walls ##
  ## Introduce variables for vertical_2_vertex and vertical_2_screen
  ## vertical_2_vertex and vertical_2_screen refer to the vertical distance
  ## between the subject's position and the the vertex of the tunnel and screen
  ## below them, respectively.
  obj_name$vertical_2_vertex <-
    abs(obj_name$perch_2_vertex) + obj_name$position_height
  obj_name$vertical_2_screen <-
    obj_name$vertical_2_vertex -
    (abs(obj_name$position_width) / tan(obj_name$vertex_angle))


  ## Introduce variables for horizontal_2_screen on positive and negative sides
  ## of the tunnel.
  ## horizontal_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  obj_name$horizontal_2_screen_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$vertical_2_screen * tan(obj_name$vertex_angle), # TRUE
           (obj_name$vertical_2_screen * tan(obj_name$vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE

  obj_name$horizontal_2_screen_neg <-
    ifelse(obj_name$position_width < 0, # if in negative side of tunnel
           obj_name$vertical_2_screen * tan(obj_name$vertex_angle), # TRUE
           (obj_name$vertical_2_screen * tan(obj_name$vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE


  ## Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen (axis of gaze is orthogonal to plane of each screen)
  obj_name$min_dist_pos <-
    obj_name$horizontal_2_screen_pos * sin(pi/2 - obj_name$vertex_angle)
  # min_dist to positive screen
  obj_name$min_dist_neg <-
    obj_name$horizontal_2_screen_neg * sin(pi/2 - obj_name$vertex_angle)
  # min_dist to negative screen


  ## When the subject is outside the boundaries created by orthogonal planes to
  ## each wall, erroneous visual angles are calculated.
  ## Therefore we must adjust min_dist values according to position_width

  ## Create variable holding the boundary values for each observation
  obj_name$bound_pos <-
    obj_name$vertical_2_vertex * tan(pi/2 - obj_name$vertex_angle)
  obj_name$bound_neg <-
    obj_name$vertical_2_vertex * -tan(pi/2 - obj_name$vertex_angle)


  obj_name$min_dist_pos <- # overwrite min_dist_pos
    ifelse(obj_name$position_width <= 0 &
             obj_name$position_width <= obj_name$bound_neg,
           # if position_width is positive and greater than the boundary value
           sqrt(obj_name$vertical_2_vertex^2 + obj_name$position_width^2),
           # return distance to vertex
           obj_name$min_dist_pos)
           # return original min_dist_pos calculation

  obj_name$min_dist_neg <-
    ifelse(obj_name$position_width >= 0 &
             obj_name$position_width >= obj_name$bound_pos,
           # if position_width is negative and smaller than the boundary value
           sqrt(obj_name$vertical_2_vertex^2 + obj_name$position_width^2),
           # return distance to vertex
           obj_name$min_dist_neg)
           # return original min_dist_neg calculation

  ## For minimum distances to end walls (assuming animal locomotes forward)
  if (obj_name$direction == 1){
    obj_name$min_dist_end <- (tunnel_length/2 - position_length)
  } else if (obj_name$direction == -1) {
    obj_name$min_dist_end <- abs(-tunnel_length/2 - position_length)
  }


  ## Leave note that minimum distances were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "min_dist_v_calculated")

  ## for simplify = TRUE
  obj_simplify$min_dist_pos <- obj_name$min_dist_pos
  obj_simplify$min_dist_neg <- obj_name$min_dist_neg
  obj_simplify$min_dist_end <- obj_name$min_dist_end

  if(simplify = TRUE){
    return(obj_simplify)
  } else {
    return(obj_name)
  }
}


## min_dist function for box shaped chamber
## I believe the ifelse statements for calculating min_dist are necessary - the
## reviewer's suggestion doesn't work as required.

calc_min_dist_box <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "treatments_added")){
    stop("Please run insert_treatments() prior to use")
  }

  ## Calculate minimum distance to each wall from positive or negative sides of
  ## tunnel
  obj_name$min_dist_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$pos_wall - obj_name$position_width, # TRUE
           obj_name$pos_wall + abs(obj_name$position_width) # FALSE
    )

  obj_name$min_dist_neg <-
    ifelse(obj_name$position_width <= 0, # if in negative side of tunnel
           abs(obj_name$neg_wall + obj_name$position_width), # TRUE
           abs(obj_name$neg_wall) + obj_name$position_width # FALSE
    )

  ## For minimum distances to end walls (assuming animal locomotes forward)
  if (obj_name$direction == 1){
    obj_name$min_dist_end <- (tunnel_length/2 - position_length)
  } else if (obj_name$direction == -1) {
    obj_name$min_dist_end <- abs(-tunnel_length/2 - position_length)
  }

  ## Leave note that minimum distances were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "min_dist_box_calculated")


  return(obj_name)
}


## get_vis_angle
## include vis_angle for end wall
get_vis_angle <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that calc_min_dist() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "min_dist_v_calculated" |
                                               "min_dist_box_calculated")){
    stop("Please calculate minimum distances prior to use")
  }

  ## Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add these variables into the dataframe.
  obj_name$vis_angle_pos_rad <-
    2*atan(obj_name$stim_param_lat_pos/(2*obj_name$min_dist_pos)) # radians
  obj_name$vis_angle_neg_rad <-
    2*atan(obj_name$stim_param_lat_neg/(2*obj_name$min_dist_neg)) # radians
  if (obj_name$direction == 1){ # when flying towards positive end wall
    obj_name$vis_angle_end_rad <-
      2*atan(obj_name$stim_param_end_pos/(2*obj_name$min_dist_end)) # radians
  }
  if (obj_name$direction == -1){ # when flying towards negative end wall
    obj_name$vis_angle_end_rad <-
      2*atan(obj_name$stim_param_end_neg/(2*obj_name$min_dist_end)) # radians


  obj_name$vis_angle_pos_deg <- rad_2_deg(obj_name$vis_angle_pos_rad) # degrees
  obj_name$vis_angle_neg_deg <- rad_2_deg(obj_name$vis_angle_neg_rad) # degrees
  obj_name$vis_angle_end_deg <- rad_2_deg(obj_name$vis_angle_end_rad)

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "vis_angles_calculated")
  return(obj_name)
}

## get_sf
## include sf for front wall?
get_sf <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## spatial frequency (cycles/rad) is the inverse of visual angle (rad/cycle)
  obj_name$sf_pos <- 1/vis_angle_pos_deg
  obj_name$sf_neg <- 1/vis_angle_neg_deg
  obj_name$sf_end <- 1/vis_angle_end_deg

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "sf_calculated")


  return(obj_name)
}

## get_tf
## include tf for end wall

get_tf <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## Check that get_velocity() has been run
  if (!any(attr(obj_name, "pathviewR_steps") == "velocity_computed")){
    stop("Please run get_velocity() prior to use")
  }

  ## Temporal frequency (cycles/second) is calculated from the axis-specific
  ## instantaneous velocity of the subject and the arc length of the cycle
  ## from the subject's perspective

  ##
  if (any(attr(obj_name, "pathviewR_steps")) == "min_dist_box_calculated"){
    # tf of positive wall from forward movement
    obj_name$tf_forward_pos <- abs(length_inst_vel)/
      (min_dist_pos*vis_angle_pos_rad)
    # tf of negative wall from forward movement
    obj_name$tf_forward_neg <- abs(length_inst_vel)/
      (min_dist_neg*vis_angle_neg_rad)
    # tf of positive wall from vertical movement
    obj_name$tf_vertical_pos <- abs(height_inst_vel)/
      (min_dist_pos*vis_angle_pos_rad)
    # tf of negative wall from vertical movement
    obj_name$tf_vertical_neg <- abs(height_inst_vel)/
      (min_dist_neg*vis_angle_neg_rad)
    # tf of end wall from horizontal movement
    obj_name$tf_horizontal_end <- abs(width_inst_vel)/
      (min_dist_end*vis_angle_end_rad)
    # tf of end wall from vertical movement
    obj_name$tf_vertical_end <- abs(height_inst_vel)/
      (min_dist_end*vis_angle_end_rad)

    # tf of length-height planar image movement from forward-vertical planar locomotion
    obj_name$lat_planar_inst_vel <- sqrt(length_inst_vel^2 + height_inst_vel^2) # locomotion
    obj_name$tf_lat_planar_pos <- abs(lat_planar_inst_vel)/
      (min_dist_pos*vis_angle_pos_rad) # tf

    # tf of the width_height planar image movement from horizontal-vertical planar locomotion
    obj_name$end_planar_inst_vel <- sqrt(width_inst_vel^2 + height_inst_vel^2) # locomotion
    obj_name$tf_end_planar <- abs(end_planar_inst_vel)/
      (min_dist_end*vis_angle_end_rad) # tf

  } else if (any(attr(obj_name, "pathviewR_steps")) == "min_dist_v_calculated"){
    # tf of positive wall from forward movement
    obj_name$tf_forward_pos <- abs(length_inst_vel)/
      (min_dist_pos*vis_angle_pos_rad)
    # tf of negative wall from forward movement
    obj_name$tf_forward_neg <- abs(length_inst_vel)/
      (min_dist_neg*vis_angle_neg_rad)
  }

  ### analysis of how multi-dimensional motion affects temporal frequency is in
  ### development

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "tf_calculated")

  return(obj_name)
}


## get_pattern_velocity
## include pattern velocity for end wall
get_pattern_vel <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## Check that get_sf() and get_tf() have been run
  if (!any(attr(obj_name, "pathviewR_steps") == "sf_calculated" |
                                                "tf_calculated")){
    stop("Please run get_sf() and get_tf() prior to use")
  }

  obj_name$pattern_vel_pos <- tf_pos/sf_pos
  obj_name$pattern_vel_neg <- tf_neg/sf_neg

  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "pattern_vel_calculated")

  return(obj_name)
}


## get image expansion
## add image expansion for end wall
get_image_expansion <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## Convert obj_name to data.frame for diff functions
  obj_name <- as.data.frame(obj_name)

  ## Get relevant frame-by-frame differences
  time_diff <- c(NA, diff(obj_name[,"time_sec"]))
  pos_angle_diff <- c(NA, diff(obj_name[,"vis_angle_pos_deg"]))
  neg_angle_diff <- c(NA, diff(obj_name[,"vis_angle_neg_deg"]))

  obj_name$expansion_pos <- pos_angle_diff/time_diff
  obj_name$expansion_neg <- neg_angle_diff/time_diff
  obj_name$time_diff <- time_diff

  ## Leave note that image expansion was calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                              "image_expansion_calculated")

  return(obj_name)
}



## all in one function for visual perceptions

get_vis_percepts <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name, "pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Run appropriate calc_min_dist based on tunnel configuration
  if (any(names(obj_name) == "vertex_angle")){
    calc_min_dist_v(obj_name)
  } else if (any(names(obj_name)) == "pos_wall"){
    calc_min_dist_box(obj_name)
  } else {
    print("Please check arguments for insert_treatments()")
  }

  ## Get visual angles, spatial frequency, temporal frequency, pattern velocity,
  ## and image expansion
get_vis_angle(obj_name)
get_sf(obj_name)
get_tf(obj_name) # need to get instantaneous velocity first
get_pattern_vel(obj_name)
get_image_expansion(obj_name)

}












