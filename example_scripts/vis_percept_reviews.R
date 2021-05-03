## work space for addressing visual perception functions reviews

## change argument for vertex_angle to mean the actual angle of the V in the
## V-shaped tunnel. Build into insert_treatments() a calculation that makes it
## work with calc_min_dist functions

## insert_treatments()
## use tunnel_length argument so that front/back wall distances can be calculated

insert_treatments <- function(obj_name,
                              tunnel_config = "box",
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
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_full_trajectories has been run prior to use
  if (!any(attr(obj_name, "pathviewr_steps") == "full_trajectories")){
    stop("Run get_full_trajectories() prior to use")
  }

  ## Translate arguments into variables at beginning of data frame
  if (tunnel_config == "v"){
    ## Check that relevant numerical arguments have positive values
    if (perch_2_vertex < 0){
      stop("perch_2_vertex must have a positive value.")
    } else if (vertex_angle < 0){
      stop("vertex_angle must have a positive value")
    } else if (tunnel_length < 0){
      stop("tunnel_length must have a positive value")
    } else if (stim_param_lat_pos < 0){
      stop("stim_param_lat_pos must have a positive value")
    } else if (stim_param_lat_neg < 0){
      stop("stim_param_lat_neg must have a positive value")
    } else if (stim_param_end_pos < 0){
      stop("stim_param_end_pos must have a positive value")
    } else if (stim_param_end_neg < 0){
      stop("stim_param_end_neg must have a positive value")
    }

    obj_name <- tibble::add_column(obj_name, .before = "frame",
                                   tunnel_config = tunnel_config,
                                   perch_2_vertex = perch_2_vertex,
                                   vertex_angle = deg_2_rad(vertex_angle/2),
                                   tunnel_length = tunnel_length,
                                   stim_param_lat_pos = stim_param_lat_pos,
                                   stim_param_lat_neg = stim_param_lat_neg,
                                   stim_param_end_pos = stim_param_end_pos,
                                   stim_param_end_neg = stim_param_end_neg,
                                   treatment = treatment)
  } else if (tunnel_config == "box"){
    ## Check that relevant numerical arguments have positive values
    if (tunnel_width < 0){
      stop("tunnel_width must have a positive value")
    } else if (tunnel_length < 0){
      stop("tunnel_length must have a positive value")
    } else if (stim_param_lat_pos < 0){
      stop("stim_param_lat_pos must have a positive value")
    } else if (stim_param_lat_neg < 0){
      stop("stim_param_lat_neg must have a positive value")
    } else if (stim_param_end_pos < 0){
      stop("stim_param_end_pos must have a positive value")
    } else if (stim_param_end_neg < 0){
      stop("stim_param_end_neg must have a positive value")
    }

    obj_name <- tibble::add_column(obj_name, .before = "frame",
                                   tunnel_config = tunnel_config,
                                   tunnel_width = tunnel_width,
                                   tunnel_length= tunnel_length,
                                   stim_param_lat_pos = stim_param_lat_pos,
                                   stim_param_lat_neg = stim_param_lat_neg,
                                   stim_param_end_pos = stim_param_end_pos,
                                   stim_param_end_neg = stim_param_end_neg,
                                   treatment = treatment)
  }

  ## Add arguments into metadata......surewhynot
  if (tunnel_config == "v"){
    attr(obj_name, "tunnel_config") <- tunnel_config
    attr(obj_name, "perch_2_vertex") <- perch_2_vertex
    attr(obj_name, "vertex_angle") <- vertex_angle
    attr(obj_name, "tunnel_width") <- tunnel_width
    attr(obj_name, "tunnel_length") <- tunnel_length
    attr(obj_name, "stim_param_lat_pos") <- stim_param_lat_pos
    attr(obj_name, "stim_param_lat_neg") <- stim_param_lat_neg
    attr(obj_name, "stim_param_end_pos") <- stim_param_end_pos
    attr(obj_name, "stim_param_end_neg") <- stim_param_end_neg
    attr(obj_name, "treatment") <- treatment
  } else if (tunnel_config == "box"){
    attr(obj_name, "tunnel_config") <- tunnel_config
    attr(obj_name, "tunnel_width") <- tunnel_width
    attr(obj_name, "tunnel_length") <- tunnel_length
    attr(obj_name, "stim_param_lat_pos") <- stim_param_lat_pos
    attr(obj_name, "stim_param_lat_neg") <- stim_param_lat_neg
    attr(obj_name, "stim_param_end_pos") <- stim_param_end_pos
    attr(obj_name, "stim_param_end_neg") <- stim_param_end_neg
    attr(obj_name, "treatment") <- treatment
  }

  ## Leave note that treatments were added
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
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
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments() has been run
  if (!any(attr(obj_name,"pathviewr_steps") == "treatments_added")){
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
  obj_name$min_dist_end <-
    ifelse(obj_name$end_length_sign == 1,
           obj_name$tunnel_length/2 - obj_name$position_length,
           obj_name$tunnel_length/2 + obj_name$position_length)


  ## for simplify = TRUE
  obj_simplify$min_dist_pos <- obj_name$min_dist_pos
  obj_simplify$min_dist_neg <- obj_name$min_dist_neg
  obj_simplify$min_dist_end <- obj_name$min_dist_end

  ## return object and add note that minimum distaces were calculated
  if(simplify == TRUE){
    attr(obj_simplify, "pathviewr_steps") <-
      c(attr(obj_name, "pathviewr_steps"), "min_dist_calculated")
    return(obj_simplify)
  } else {
    attr(obj_name, "pathviewr_steps") <-
      c(attr(obj_name, "pathviewr_steps"), "min_dist_calculated")
    return(obj_name)
  }
}


## min_dist function for box shaped chamber

calc_min_dist_box <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments() has been run
  if (!any(attr(obj_name,"pathviewr_steps") == "treatments_added")){
    stop("Please run insert_treatments() prior to use")
  }

  ## Calculate minimum distance to each wall from positive or negative sides of
  ## tunnel
  obj_name$min_dist_pos <- obj_name$tunnel_width/2 - obj_name$position_width
  obj_name$min_dist_neg <- obj_name$tunnel_width/2 + obj_name$position_width

  ## For minimum distances to end walls (assuming animal locomotes forward)
  obj_name$min_dist_end <-
    ifelse(obj_name$end_length_sign == 1,
           obj_name$tunnel_length/2 - obj_name$position_length,
           obj_name$tunnel_length/2 + obj_name$position_length)

  ## Leave note that minimum distances were calculated
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                         "min_dist_calculated")


  return(obj_name)
}


## get_vis_angle
## include vis_angle for end wall
get_vis_angle <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that calc_min_dist() has been run
  if (!any(attr(obj_name, "pathviewr_steps") == "min_dist_calculated")){
    stop("Please run calc_min_dist_v() or calc_min_dist_box() prior to use")
  }

  ## Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add these variables into the dataframe.
  # visual angle to positive wall
  obj_name$vis_angle_pos_rad <-
    2*atan(obj_name$stim_param_lat_pos/(2*obj_name$min_dist_pos))
  # visual angle to negative wall
  obj_name$vis_angle_neg_rad <-
    2*atan(obj_name$stim_param_lat_neg/(2*obj_name$min_dist_neg))
  # visual angle to end wall (depending on direction of locomotion)
  obj_name$vis_angle_end_rad <-
    ifelse(obj_name$end_length_sign == 1,
           2*atan(obj_name$stim_param_end_pos/(2*obj_name$min_dist_end)),
           2*atan(obj_name$stim_param_end_neg/(2*obj_name$min_dist_end)))

  # convert to degrees
  obj_name$vis_angle_pos_deg <- rad_2_deg(obj_name$vis_angle_pos_rad)
  obj_name$vis_angle_neg_deg <- rad_2_deg(obj_name$vis_angle_neg_rad)
  obj_name$vis_angle_end_deg <- rad_2_deg(obj_name$vis_angle_end_rad)

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                         "vis_angles_calculated")
  return(obj_name)
  }

## get_sf
## include sf for front wall?
get_sf <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewr_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## spatial frequency (cycles/rad) is the inverse of visual angle (rad/cycle)
  obj_name$sf_pos <- 1/obj_name$vis_angle_pos_deg
  obj_name$sf_neg <- 1/obj_name$vis_angle_neg_deg
  obj_name$sf_end <- 1/obj_name$vis_angle_end_deg

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                         "sf_calculated")


  return(obj_name)
}

## get_tf

get_tf <- function(obj_name,
                   dimensionality = NULL){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewr_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## Check that get_velocity() has been run
  if (!any(attr(obj_name, "pathviewr_steps") == "velocity_computed")){
    stop("Please run get_velocity() prior to use")
  }


  ## Temporal frequency (cycles/second)instantaneous velocity of the subject and
  ## the arc length of the cycle from the subject's perspective

  ## For box-shaped tunnel
   if (attr(obj_name, "tunnel_config") == "box"){

        ## Lateral wall 1D (axial) temporal frequencies
    # tf of positive wall from forward movement
    obj_name$tf_forward_pos <-
      abs(obj_name$length_inst_vel)/
      (obj_name$min_dist_pos * obj_name$vis_angle_pos_rad)
    # tf of negative wall from forward movement
    obj_name$tf_forward_neg <-
      abs(obj_name$length_inst_vel)/
      (obj_name$min_dist_neg * obj_name$vis_angle_neg_rad)
    # tf of positive wall from vertical movement
    obj_name$tf_vertical_pos <-
      abs(obj_name$height_inst_vel)/
      (obj_name$min_dist_pos * obj_name$vis_angle_pos_rad)
    # tf of negative wall from vertical movement
    obj_name$tf_vertical_neg <-
      abs(obj_name$height_inst_vel)/
      (obj_name$min_dist_neg * obj_name$vis_angle_neg_rad)
    # tf of end wall from horizontal movement

        ## End wall 1D (axial) temporal frequencies
    # tf of end wall from horizontal movement
    obj_name$tf_horizontal_end <-
      abs(obj_name$width_inst_vel)/
      (obj_name$min_dist_end * obj_name$vis_angle_end_rad)
    # tf of end wall from vertical movement
    obj_name$tf_vertical_end <-
      abs(obj_name$height_inst_vel)/
      (obj_name$min_dist_end * obj_name$vis_angle_end_rad)

        ## Lateral wall 2D (planar) temporal frequencies
    # relevant locomotion
    obj_name$lat_planar_inst_vel <-
      sqrt(obj_name$length_inst_vel^2 + obj_name$height_inst_vel^2)
    # planar tf from positive wall
    obj_name$tf_planar_pos <-
      abs(obj_name$lat_planar_inst_vel)/
      (obj_name$min_dist_pos * obj_name$vis_angle_pos_rad)
    # planar tf from negative wall
    obj_name$tf_planar_neg <-
      abs(obj_name$lat_planar_inst_vel)/
      (obj_name$min_dist_neg * obj_name$vis_angle_neg_rad)

        ## End wall 2d (planar) temporal frequency
    # relevant locomotion
    obj_name$end_planar_inst_vel <-
      sqrt(obj_name$width_inst_vel^2 + obj_name$height_inst_vel^2)
    # planar tf from end wall
    obj_name$tf_planar_end <-
      abs(obj_name$end_planar_inst_vel)/
      (obj_name$min_dist_end * obj_name$vis_angle_end_rad)

    ## for v-shaped tunnel
  } else if (attr(obj_name, "tunnel_config") == "v"){
        ## Lateral wall 1D (axial) temporal frequencies
    # tf of positive wall from forward movement
    obj_name$tf_forward_pos <-
      abs(obj_name$length_inst_vel)/
      (obj_name$min_dist_pos * obj_name$vis_angle_pos_rad)
    # tf of negative wall from forward movement
    obj_name$tf_forward_neg <-
      abs(obj_name$length_inst_vel)/
      (obj_name$min_dist_neg * obj_name$vis_angle_neg_rad)

      ## End wall 1D (axial) temporal frequencies
    # tf of end wall from horizontal movement
    obj_name$tf_horizontal_end <-
      abs(obj_name$width_inst_vel)/
      (obj_name$min_dist_end * obj_name$vis_angle_end_rad)
    # tf of end wall from vertical movement
    obj_name$tf_vertical_end <-
      abs(obj_name$height_inst_vel)/
      (obj_name$min_dist_end * obj_name$vis_angle_end_rad)

      ## End wall 2d (planar) temporal frequency
    # relevant locomotion
    obj_name$end_planar_inst_vel <-
      sqrt(obj_name$width_inst_vel^2 + obj_name$height_inst_vel^2)
    # planar tf from end wall
    obj_name$tf_planar_end <-
      abs(obj_name$end_planar_inst_vel)/
      (obj_name$min_dist_end * obj_name$vis_angle_end_rad)

  }

  ### analysis of how multi-dimensional motion affects temporal frequency is in
  ### development for V-shaped tunnel configurations

  ## Leave a note that temporal frequencies  were calculated
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                         "tf_calculated")

  return(obj_name)
}


## get_pattern_velocity
## include pattern velocity for end wall
get_pattern_vel <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewr_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## Check that get_sf() and get_tf() have been run
  if (!any(attr(obj_name, "pathviewr_steps") == "sf_calculated")){
    stop("Please run get_sf() prior to use")
  } else if (!any(attr(obj_name, "pathviewr_steps") == "tf_calculated")){
    stop("Please run get_tf() prior to use")
  }

  ## For box-shaped tunnel configuration
  if (attr(obj_name, "tunnel_config") == "box"){
    ## Lateral wall 1D (axial) pattern velocities
  # forward pattern velocity from positive wall
  obj_name$forward_pattern_vel_pos <-
    obj_name$tf_forward_pos/obj_name$sf_pos
  # forward pattern velocity from negative wall
  obj_name$forward_pattern_vel_neg <-
    obj_name$tf_forward_neg/obj_name$sf_neg
  # vertical pattern velocity from positive wall
  obj_name$vertical_pattern_vel_pos <-
    obj_name$tf_vertical_pos/obj_name$sf_pos
  # vertical pattern velocity from negative wall
  obj_name$vertical_pattern_vel_neg <-
    obj_name$tf_vertical_neg/obj_name$sf_neg

      ## End wall 1D (axial) pattern velocities
  # horizontal pattern velocity from end wall
  obj_name$horizontal_pattern_vel_end <-
    obj_name$tf_horizontal_end/obj_name$sf_end
  # vertical pattern velocity from end wall
  obj_name$vertical_pattern_vel_end <-
    obj_name$tf_vertical_end/obj_name$sf_end

      ## Lateral wall 2D (planar) pattern velocities
  # planar pattern velocity from positive wall
  obj_name$planar_pattern_vel_pos <- o
  bj_name$tf_planar_pos/obj_name$sf_pos
  # planar pattern velocity from negative wall
  obj_name$planar_pattern_vel_neg <-
    obj_name$tf_planar_neg/obj_name$sf_neg

      ## End wall 2D (planar) pattern velocity
  # planar pattern velocity from positive wall
  obj_name$planar_pattern_vel_end <-
    obj_name$tf_planar_end/obj_name$sf_end

  ## For v-shaped tunnel configuration
  } else if (attr(obj_name, "tunnel_config") == "v"){
      ## Lateral wall 1D (axial) pattern velocities
    # forward pattern velocity for positive wall
    obj_name$forward_pattern_vel_pos <-
      obj_name$tf_forward_pos/obj_name$sf_pos
    # forward pattern velocity for negative wall
    obj_name$forward_pattern_vel_neg <-
      obj_name$tf_forward_neg/obj_name$sf_neg

    ## End wall 1D (axial) pattern velocities
    # horizontal pattern velocity from end wall
    obj_name$horizontal_pattern_vel_end <-
      obj_name$tf_horizontal_end/obj_name$sf_end
    # vertical pattern velocity from end wall
    obj_name$vertical_pattern_vel_end <-
      obj_name$tf_vertical_end/obj_name$sf_end

    ## End wall 2D (planar) pattern velocity
    # planar pattern velocity from positive wall
    obj_name$planar_pattern_vel_end <-
      obj_name$tf_planar_end/obj_name$sf_end


  }

  ## Leave a note that pattern velocities were calculated
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                         "pattern_vel_calculated")

  return(obj_name)
}


## get image expansion
get_image_expansion <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewr_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## Convert obj_name to data.frame for diff functions
  obj_name <- as.data.frame(obj_name)

  ## Get relevant frame-by-frame differences
  time_diff <- c(NA, diff(obj_name[,"time_sec"]))
  pos_angle_diff <- c(NA, diff(obj_name[,"vis_angle_pos_deg"]))
  neg_angle_diff <- c(NA, diff(obj_name[,"vis_angle_neg_deg"]))
  end_angle_diff <- c(NA, diff(obj_name[,"vis_angle_end_deg"]))

  obj_name$expansion_pos <- pos_angle_diff/time_diff
  obj_name$expansion_neg <- neg_angle_diff/time_diff
  obj_name$expansion_end <- end_angle_diff/time_diff
  obj_name$time_diff <- time_diff

  ## Leave note that image expansion was calculated
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                              "image_expansion_calculated")

  return(as_tibble(obj_name))
}

# get pattern velocity direction
# All functions, including this one assume the bird is looking straight ahead
# (i.e. a "rightwards" flight means the bird is always facing "rightwards" etc.)
# Once head orientation is included, this function will include an offset equal
# to the head orientation for each observation

get_pattern_vel_direction <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewr_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_velocity() has been run
  if (!any(attr(obj_name, "pathviewr_steps") == "velocity_computed")){
    stop("Please run get_velocity() prior to use")
  }

  ## For lateral walls, forward pattern velocity is 0 degree/360 degree and intermediate
  ## angles fill the circle counter-clockwise.
  ## The direction of pattern velocity depends on the end wall to which the
  ## subject is facing and whether it's moving forward or backward.

  ## for box-shaped tunnel configurations
  if (attr(obj_name, "tunnel_config") == "box"){
  obj_name$lat_pattern_direction <-
    ifelse(obj_name$end_length_sign == 1,
           # when facing positive end of tunnel
           ifelse(obj_name$length_inst_vel >= 0,
                  # when moving forward
                  180 + rad_2_deg(atan(
                                    obj_name$height_inst_vel/
                                    obj_name$length_inst_vel)),
                 # when moving backward
                  (360 + rad_2_deg(atan(
                                    obj_name$height_inst_vel/
                                    obj_name$length_inst_vel)))) %% 360,

           # when facing negative end of tunnel
           ifelse(obj_name$length_inst_vel < 0,
                  # when moving forward
                  180 - rad_2_deg(atan(
                                    obj_name$height_inst_vel/
                                    obj_name$length_inst_vel)),
                  # when moving backward
                  (360 - rad_2_deg(atan(
                                    obj_name$height_inst_vel/
                                    obj_name$length_inst_vel)))) %% 360
    )

  ## For end wall, rightward pattern velocity is 0 degree or 360 degree and intermediate
  ## angles fill the circle counter-clockwise.
  ## The direction of pattern velocity depends on the end wall to which the
  ## subject is facing and whether it's moving towards the positive or negative
  ## lateral walls.

  obj_name$end_pattern_direction <-
    ifelse(obj_name$end_length_sign == 1,
           # when facing positive end of tunnel
           ifelse(obj_name$width_inst_vel >= 0,
                 # when moving rightwards
                 180 + rad_2_deg(atan(
                                  obj_name$height_inst_vel/
                                  obj_name$width_inst_vel)),
                 # when moving leftwards
                 (360 + rad_2_deg(atan(
                                  obj_name$height_inst_vel/
                                  obj_name$width_inst_vel)))) %% 360,

           # when facing negative end of tunnel
           ifelse(obj_name$width_inst_vel < 0,
                  # when moving rightwards
                  180 - rad_2_deg(atan(
                                  obj_name$height_inst_vel/
                                  obj_name$width_inst_vel)),
                  # when moving leftwards
                  (360 - (rad_2_deg(atan(
                                  obj_name$width_inst_vel/
                                  obj_name$length_inst_vel))))) %% 360
    )} else if (
       ## For v-shaped tunnel configurations
    attr(obj_name, "tunnel_config") == "v"){
      ## For end wall, rightward pattern velocity is 0 degree or 360 degree and intermediate
      ## angles fill the circle counter-clockwise.
      ## The direction of pattern velocity depends on the end wall to which the
      ## subject is facing and whether it's moving towards the positive or negative
      ## lateral walls.

      obj_name$end_pattern_direction <-
        ifelse(obj_name$end_length_sign == 1,
               # when facing positive end of tunnel
               ifelse(obj_name$width_inst_vel >= 0,
                      # when moving rightwards
                      180 + rad_2_deg(atan(
                        obj_name$height_inst_vel/
                          obj_name$width_inst_vel)),
                      # when moving leftwards
                      (360 + rad_2_deg(atan(
                        obj_name$height_inst_vel/
                          obj_name$width_inst_vel)))) %% 360,

               # when facing negative end of tunnel
               ifelse(obj_name$width_inst_vel < 0,
                      # when moving rightwards
                      180 - rad_2_deg(atan(
                        obj_name$height_inst_vel/
                          obj_name$width_inst_vel)),
                      # when moving leftwards
                      (360 - (rad_2_deg(atan(
                        obj_name$width_inst_vel/
                          obj_name$length_inst_vel))))) %% 360
        )
    }

  ## Leave a note that pattern velocity direction was calculated
  attr(obj_name, "pathviewr_steps") <- c(attr(obj_name, "pathviewr_steps"),
                                         "pattern_vel_direction_calculated")

  return(obj_name)
}





## all in one function for visual perceptions

get_vis_percepts <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name, "pathviewr_steps") == "viewr")){
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




