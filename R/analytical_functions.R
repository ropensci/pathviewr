## Part of the pathviewR package
## Last updated: 2020-07-02 VBB

################################## get_velocity ################################
## Get instantaneous velocity for all subjects
##
## do we need to adjust to account for frame/time gaps?

get_velocity <- function(obj_name,
                         time_col = "time_sec",
                         length_col = "position_length",
                         width_col = "position_width",
                         height_col = "position_height",
                         add_to_viewr = TRUE,
                         velocity_min = NA,
                         velocity_max = NA,
                         ...) {

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Argument checks
  if (!any(grepl(time_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("time_col not found.
Please check that you have entered the name of the time variable correctly.")
  }

  if (!any(grepl(length_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("length_col not found.
Please check that you have entered the name of the length variable correctly.")
  }

  if (!any(grepl(width_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("width_col not found.
Please check that you have entered the name of the width variable correctly.")
  }

  if (!any(grepl(height_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("height_col not found.
Please check that you have entered the name of the height variable correctly.")
  }

  ## Convert to data.frame for easier column extraction
  df <- as.data.frame(obj_name)

  ## Create vector of time differences
  time_diffs <- diff(df[,time_col])

  ## Calculate instantaneous velocities
  ## First entry is set to zero because calculating velocity depends on previous
  ## step. We may opt change this to `NA` later on so that these can be filtered
  ## easily.
  length_inst <- c(0, diff(df[,length_col]) / time_diffs)
  width_inst  <- c(0, diff(df[,width_col])  / time_diffs)
  height_inst <- c(0, diff(df[,height_col]) / time_diffs)

  ## Calculate object velocity
  vel <- sqrt((length_inst ^ 2) + (width_inst ^ 2) + (height_inst ^ 2))

  ## Combine
  res <- tibble::tibble(velocity = vel,
                        length_inst_vel = length_inst,
                        width_inst_vel = width_inst,
                        height_inst_vel = height_inst
                        )

  ## If add_to_viewr is TRUE, add it as new columns to the input viewr object.
  ## If add_to_viewr is FALSE, export the standalone (res) tibble
  if (add_to_viewr == TRUE){
    ## Add the new columns and generate a new viewr object
    obj_new <- bind_cols(obj_name, res)

    ## Leave a note that we computed velocity via get_velocity()
    attr(obj_new,"pathviewR_steps") <-
      c(attr(obj_name,"pathviewR_steps"), "velocity_computed")

  } else { ## if FALSE
    obj_new <- res
  }

  #let's add some threshold arguments to set biologically reasonable limits
  #velocity_min
  if (is.numeric(velocity_min)){
    ## filter velocity
    obj_new <- obj_new %>%
      filter(velocity > velocity_min)

    ## Leave a note set velocity_min via get_velocity()
    #leave note even if not added to viewr object?
    attr(obj_new,"velocity_min") <- velocity_min

  } else { ## if FALSE
    obj_new <- obj_new
    #if is character instead of numeric:
    if (is.character(velocity_min)) {
      stop("velocity_min is character.
    Please check that you have entered the velocity_min variable correctly.")
    }
  }
  #velocity_max
  if (is.numeric(velocity_max)){
    ## filter velocity
    obj_new <- obj_new %>%
      filter(velocity < velocity_max)

    ## Leave a note set velocity_min via get_velocity()
    #leave note even if not added to viewr object?
    attr(obj_new,"velocity_max") <- velocity_max

  } else { ## if FALSE
    obj_new <- obj_new
    #if is character instead of numeric:
    if (is.character(velocity_max)) {
      stop("velocity_max is character.
    Please check that you have entered the velocity_max variable correctly.")
    }
  }

  ## Output
  return(obj_new)

}

############################## get_dist_point_line_2d ##########################
## Compute the distance between a point and a line in a 2D space (i.e. on an
## XY plane)
##
## NOTE: CANNOT SET DEFAULTS TO c(0, 0) AS IT MESSES UP THE FUNCTION INTERNALLY

get_dist_point_line_2d <- function(point,
                                   line_coord1,
                                   line_coord2) {
  v1 <- line_coord1 - line_coord2
  v2 <- point - line_coord1
  m <- cbind(v1, v2)
  dist <- abs(det(m)) / sqrt(sum(v1 * v1))
  return(dist)
}


################################ get_3dcross_prod ##############################
## Compute the cross product of 3D vectors
## Will fill in details later

get_3dcross_prod <- function(v1,
                             v2){
  v3 <- vector()
  v3[1] <- v1[2] * v2[3] - v1[3] * v2[2]
  v3[2] <- v1[3] * v2[1] - v1[1] * v2[3]
  v3[3] <- v1[1] * v2[2] - v1[2] * v2[1]
  return(v3)
}

############################## get_dist_point_line_3d ##########################
## Compute the distance between a point and a line in 3D space
## Will fill in details later

get_dist_point_line_3d <- function(point,
                                   line_coord1,
                                   line_coord2) {
  v1 <- line_coord1 - line_coord2
  v2 <- point - line_coord1
  v3 <- get_3dcross_prod(v1, v2)
  area <- sqrt(sum(v3 * v3)) / 2
  dist <- 2 * area / sqrt(sum(v1 * v1))
  return(dist)
}


#################################### rad2deg ###################################
## convert radians to degrees
rad2deg <- function(rad) {

  ## Check that it's a numeric
  if (!any(class(rad) == "numeric")) {
    stop("Input angle must be a numeric")
  }

  return((rad * 180) / (pi))

}


#################################### deg2rad ###################################
## convert degrees to radians
deg2rad <- function(deg) {

  ## Check that it's a numeric
  if (!any(class(deg) == "numeric")) {
    stop("Input angle must be a numeric")
  }

  return((deg * pi) / (180))

}


#################        calc_vis_angle       ###################

#' Based on rigid body, i.e animal head positions in a "V" shaped tunnel,
#' \code{calc_vis_angle()} calculates the visual angles created by lateral
#' visual stimuli.
#'
#' @param obj_name A tibble or data.frame with attribute \code{viewr}
#' @param vertex_angle The angle (in degrees) subtended by a vertical axis and
#' the sides of the tunnel. Equivalent to the angle of the "V" divided by 2.
#' \code{vertex_angle)} defaults to 45.
#' @param gnd_plane The vertical distance (in meters) between the bottom of the
#' "V" and the horizontal plane through the origin (0,0,0).
#' @param stim_param_pos The length (in meters) of the visual stimulus presented
#' on the positive side of the tunnel (i.e. \code{position_width >= 0}). For
#' example, a sinusoidal grating 10cm wide is \code{stim_param_pos = 0.1}
#' @param stim_param_neg The same convention as \code{stim_param_pos} but for
#' stimuli presented on the negative side of the tunnel
#' (i.e. \code{position_width < 0}).
#'
#' @details \code{cal_vis_angle} assumes fixed gaze at the point on the
#' either side of the tunnel that minimizes the distance to visual stimuli and
#' thereby maximizes visual angles. Currently, visual angles are overestimated
#' when \code{position_width} lies outside the boundaries starting at the
#' bottom of the "V" and extend at right angles from the planes created by
#' either side of the tunnel. As \code{vertex_angle} increases from 45 to 90,
#' this boundary where calc_vis_angle is accurate becomes more restrictive.
#'
#' @return A tibble or data.frame with added variables for
#' \code{vis_angle_pos_deg} and \code{vis_angle_neg_deg} reported in degrees.
#'
#' @author Eric R. Press
#'
#' @family analytical functions
#'
#' @examples
#'
#' @export


## Assuming gaze is fixed at the point on each screen such that the axis of gaze
## is orthogonal to the plane of each screen. This function minimizes the
## distance to the screen closer to the bird and therefore maximizes the visual
## angle calculated for the closer screen.

calc_vis_angle <- function(obj_name,
                               gnd_plane,
                               stim_param_pos,
                               stim_param_neg,
                               vertex_angle = 45,
                               simplify_output = FALSE){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Translate vertex_angle from degrees to radians for trig functions
  deg2rad(vertex_angle)

  ## duplicate object for simplify = TRUE
  obj_simplify <- obj_name

  ## Introduce variables for height_2_vertex and height_2_screen
  obj_name <- obj_name %>%
    mutate(height_2_vertex = gnd_plane + position_height,
           height_2_screen = height_2_vertex -
             (abs(position_width) / tan(vertex_angle)))


  ## Introduce variables for width_2_screen on positive and negative sides
  ## of the tunnel using ifelse()
  ## width_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  obj_name$width_2_screen_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$height_2_screen * tan(vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE

  obj_name$width_2_screen_neg <-
    ifelse(obj_name$position_width < 0,# if in negative side of tunnel
           obj_name$height_2_screen * tan(vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE

  ## Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen (axis of gaze is orthogonal to plane of each screen, i.e. 45˚ down
  ## from horizontal)
  obj_name$min_dist_pos <-
    obj_name$width_2_screen_pos * sin((pi/2) - vertex_angle)
  # min_dist to positive screen

  obj_name$min_dist_neg <-
    obj_name$width_2_screen_neg * sin((pi/2) - vertex_angle)
  # min_dist to negative screen


  ## Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add these variables into the dataframe.
  obj_name <- obj_name %>%
    mutate(vis_angle_pos_rad =  2*atan(stim_param_pos/(2*obj_name$min_dist_pos)),
           # radians
           vis_angle_neg_rad =  2*atan(stim_param_neg/(2*obj_name$min_dist_neg)),
           # radians
           vis_angle_pos_deg =  rad2deg(vis_angle_pos_rad),
           # convert to deg
           vis_angle_neg_deg =  rad2deg(vis_angle_neg_rad))
           # convert to deg

  obj_simplify <- obj_simplify %>%
    mutate(vis_angle_pos_deg =  rad2deg(obj_name$vis_angle_pos_rad),
           vis_angle_neg_deg =  rad2deg(obj_name$vis_angle_neg_rad))

  if(simplify_output == TRUE){
    return(obj_simplify)
  } else {
    return(obj_name)
    }
}





## Remaining three functions originally written by Christina Harvey
## May adapt for our purposes in the future
#angles in 3D space
xyzangles <- function(x1,y1,z1,x2,y2,z2,x3,y3,z3){
  i1=x2-x1
  i2=x2-x3
  j1=y2-y1
  j2=y2-y3
  k1=z2-z1
  k2=z2-z3
  dotprod=(i1*i2)+(j1*j2)+(k1*k2)
  len1=sqrt(i1^2+j1^2+k1^2)
  len2=sqrt(i2^2+j2^2+k2^2)
  theta=acos(dotprod/(len1*len2))*(180/pi)
  theta
}


#angles in 2D space
xyangles<-function(x1,y1,x2,y2,x3,y3){
  i1=x1;i2=x2;i3=x3
  j1=y1;j2=y2;j3=y3
  a=c(i1,j1)-c(i2,j2)
  b=c(i3,j3)-c(i2,j2)
  theta=acos(sum(a*b)/(sqrt(sum(a*a))*sqrt(sum(b*b))))*(180/pi)
  theta
}


#angles between two planes in 3D space
xyzplaneangles <- function(x1,y1,z1,x2,y2,z2,x3,y3,z3, #plane 1
                           x4,y4,z4,x5,y5,z5,x6,y6,z6){ #plane 2
  #x1_std,y1_std,z1_std,x2_std,y2_std,z2_std,x3_std,y3_std,z3_std){
  #This computes the angle betweeon two planes composed of three points each
  #--- Vectors of Plane 1 - create vector from two 3D pts
  i1 = x2-x1
  j1 = y2-y1
  k1 = z2-z1
  i2 = x2-x3
  j2 = y2-y3
  k2 = z2-z3
  #--- Vectors of Plane 2 - create vector from two 3D pts
  i3 = x5-x4
  j3 = y5-y4
  k3 = z5-z4
  i4 = x5-x6
  j4 = y5-y6
  k4 = z5-z6
  #--- Orthogonal Vector of Plane 1 - Cross Product of the vectors on the plane
  i5 = j1*k2-k1*j2
  j5 = k1*i2-i1*k2
  k5 = i1*j2-j1*i2
  #--- Orthogonal Vector of Plane 2 - Cross Product of the vectors on the plane
  i6 = j3*k4-k3*j4
  j6 = k3*i4-i3*k4
  k6 = i3*j4-j3*i4
  #--- Dot Product of Orthogonal Vectors
  dotprod = ((i5*i6)+(j5*j6)+(k5*k6))
  len1    = sqrt(i5^2+j5^2+k5^2)
  len2    = sqrt(i6^2+j6^2+k6^2)
  interior = dotprod/(len1*len2)
  theta   = acos(interior)*(180/pi)

  return(theta)
}

