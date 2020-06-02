## Part of the pathviewR package
## Last updated: 2020-06-02 VBB

################################ get_rb_velocity ###############################
## Get instantaneous velocity for rigid body objects

get_rb_velocity <- function(obj_name,
                            time_col,
                            x_col,
                            y_col,
                            z_col,
                            ...) {
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }

  df <- as.data.frame(obj_name) # convert back to standard df?

  ## Create vector of time differences
  time_diffs <- diff(df[,time_col])

  ## Calculate instantaneous velocities
  ## First entry is set to zero because calculating velocity depends on previous
  ## step. We may opt change this to `NA` later on so that these can be filtered
  ## easily.
  x_inst <- c(0, diff(df[,x_col]) / time_diffs)
  y_inst <- c(0, diff(df[,y_col]) / time_diffs)
  z_inst <- c(0, diff(df[,z_col]) / time_diffs)

  ## Calculate object velocity
  vel <- sqrt((x_inst ^ 2) + (y_inst ^ 2) + (z_inst ^ 2))

  ## Combine
  res <- tibble::tibble(velocity = vel, x_inst, y_inst, z_inst)

  ## Output
  return(res)
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


#################        calc_vis_angle_mod       ###################

## Assuming gaze is fixed at the point on each screen such that the axis of gaze
## is orthogonal to the plane of each screen. This function minimizes the distance
## to the screen closer to the bird and therefore maximizes the visual angle
## calculated for the closer screen.
## However, this does not hold true for the opposite screen in the following
## condition. If the vertex angle of the tunnel is > 45˚ and the bird's position
## extends beyond the boundary created by a 45˚ vertex angle (the bird is quite
## close to one screen), then the closest point to the opposite screen is the
## vertex itself. Given that the birds tend to fly fairly close to the center of
## the tunnel, this may not be encountered very often.

calc_vis_angle_mod <- function(obj_name,
                               vertex_angle = 45,
                               gnd_plane,
                               stim_param_pos,
                               stim_param_neg){


  ## Part 1. Translate vertex_angle from degrees to radians for trig functions
  vertex_angle <- (vertex_angle * pi) / 180


  ## Part 2. Introduce variables for height_2_vertex and height_2_screen
  obj_name <- obj_name %>%
    mutate(height_2_vertex = gnd_plane + Position_heights,
           height_2_screen = height_2_vertex -
             (abs(Position_widths) / tan(vertex_angle)))


  ## Part 3. Introduce variables for width_2_screen on positive and negative sides
  ## of the tunnel using ifelse() which combines a conditional and a for loop.
  ## width_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  obj_name$width_2_screen_pos <-
    ifelse(obj_name$Position_widths >= 0,
           # if bird is in positive side of tunnel
           obj_name$height_2_screen * tan(vertex_angle),
           # TRUE = width_2_screen_pos
           (obj_name$height_2_screen * tan(vertex_angle)) +
             (2 * abs(obj_name$Position_widths)))
  # FALSE = width_2_screen_pos + 2 * abs(Position_widths)

  obj_name$width_2_screen_neg <-
    ifelse(obj_name$Position_widths < 0,
           # if bird is in negative side of tunnel
           obj_name$height_2_screen * tan(vertex_angle),
           # TRUE = width_2_screen_neg
           (obj_name$height_2_screen * tan(vertex_angle)) +
             (2 * abs(obj_name$Position_widths)))
  # FALSE = width_2_screen_neg + 2 * abs(Position_widths)

  ## Part 4. Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen (axis of gaze is orthogonal to plane of each screen, i.e. 45˚ down
  ## from horizontal)
  obj_name$min_dist_pos <-
    obj_name$width_2_screen_pos * sin((pi/2) - vertex_angle)
  # min_dist to positive screen

  obj_name$min_dist_neg <-
    obj_name$width_2_screen_neg * sin((pi/2) - vertex_angle)
  # min_dist to negative screen


  ## Part 5. Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add these variables into the dataframe.
  obj_name <- obj_name %>%
    mutate(vis_angle_pos_rad =  2*atan(stim_param_pos/(2*obj_name$min_dist_pos)),
           # radians
           vis_angle_neg_rad =  2*atan(stim_param_neg/(2*obj_name$min_dist_neg)),
           # radians
           vis_angle_pos_deg =  vis_angle_pos_rad * (180 / pi),
           # convert to deg
           vis_angle_neg_deg =  vis_angle_neg_rad * (180 / pi))
  # convert to deg

  return(obj_name)
}




#################        calc_vis_angle_mod.1       ###################

## Same as above but without adding all internal variables to the dataframe

calc_vis_angle_mod.1 <- function(obj_name,
                                 vertex_angle = 45,
                                 gnd_plane,
                                 stim_param_pos,
                                 stim_param_neg){


  ## Part 1. Translate vertex_angle from degrees to radians for trig functions
  vertex_angle <- (vertex_angle * pi) / 180


  ## Part 2. Introduce variables for height_2_vertex and height_2_screen
  height_2_vertex <- gnd_plane + obj_name$Position_heights
  height_2_screen <- height_2_vertex -
    (abs(obj_name$Position_widths) / tan(vertex_angle))


  ## Part 3. Introduce variables for width_2_screen on positive and negative sides
  ## of the tunnel using ifelse() which combines a conditional and a for loop.
  ## width_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  width_2_screen_pos <-
    ifelse(obj_name$Position_widths >= 0,
           # if bird is in positive side of tunnel
           height_2_screen * tan(vertex_angle),
           # TRUE = width_2_screen_pos
           height_2_screen * tan(vertex_angle) +
             (2 * abs(obj_name$Position_widths)))
  # FALSE = width_2_screen_pos + 2 * abs(Position_widths)

  width_2_screen_neg <-
    ifelse(obj_name$Position_widths < 0,
           # if bird is in negative side of tunnel
           height_2_screen * tan(vertex_angle),
           # TRUE = width_2_screen_neg
           height_2_screen * tan(vertex_angle) +
             (2 * abs(obj_name$Position_widths)))
  # FALSE = width_2_screen_neg + 2 * abs(Position_widths)


  ## Part 4. Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen (axis of gaze is orthogonal to plane of each screen, i.e. 45˚ down
  ## from horizontal)
  min_dist_pos <- width_2_screen_pos * sin((pi/2) - vertex_angle)
  # min_dist to positive screen

  min_dist_neg <- width_2_screen_neg * sin((pi/2) - vertex_angle)
  # min_dist to negative screen


  ## Part 5. Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add the degrees variables into the dataframe.
  vis_angle_pos_rad <- 2 * atan(stim_param_pos / (2 * min_dist_pos)) # radians
  vis_angle_neg_rad <- 2 * atan(stim_param_neg / (2 * min_dist_neg)) # radians

  obj_name <- obj_name %>%
    mutate(vis_angle_pos_deg =  vis_angle_pos_rad * (180 / pi), # convert to deg
           vis_angle_neg_deg =  vis_angle_neg_rad * (180 / pi)) # convert to deg

  return(obj_name)
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

