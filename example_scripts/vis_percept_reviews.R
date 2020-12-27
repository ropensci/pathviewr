## work space for addressing visual perception functions reivews
## Things to address:

# min_dist function for V-shaped tunnel
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

  ## Leave note that minimum distances were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "min_dist_calculated")

  ## for simplify = TRUE
  obj_simplify$min_dist_pos <- obj_name$min_dist_pos
  obj_simplify$min_dist_neg <- obj_name$min_dist_neg

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

  ## Leave note that minimum distances were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "min_dist_calculated")


  return(obj_name)
}


## get_vis_angle
get_vis_angle <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that calc_min_dist() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "min_dist_calculated")){
    stop("Please calculate minimum distances prior to use")
  }

  ## Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add these variables into the dataframe.
  obj_name$vis_angle_pos_rad <-
    2*atan(obj_name$stim_param_pos/(2*obj_name$min_dist_pos)) # radians
  obj_name$vis_angle_neg_rad <-
    2*atan(obj_name$stim_param_neg/(2*obj_name$min_dist_neg)) # radians

  obj_name$vis_angle_pos_deg <- rad_2_deg(obj_name$vis_angle_pos_rad) # degrees
  obj_name$vis_angle_neg_deg <- rad_2_deg(obj_name$vis_angle_neg_rad) # degree

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "vis_angles_calculated")
  return(obj_name)
}

## get_sf

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

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "sf_calculated")


  return(obj_name)
}

get_tf <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_vis_angle() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "vis_angles_calculated")){
    stop("Please run get_vis_angle() prior to use")
  }

  ## Temporal frequency (cycles/second) is calculated from the axis-specific
  ## instantaneous velocity of the subject and the arc length of the cycle
  ## from the subject's perspective
  obj_name$tf_pos <- abs(length_inst_vel)/(min_dist_pos*vis_angle_pos_rad)
  obj_name$tf_neg <- abs(length_inst_vel)/(min_dist_neg*vis_angle_neg_rad)

  ## Leave a note that visual angles were calculated
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "tf_calculated")

  return(obj_name)
}


## get_pattern_velocity

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









