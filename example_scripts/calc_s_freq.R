## In visual perception studies, spatial frequency is often expressed as the
## number of cycles per degree of visual angle. This function builds on
## calc_vis_angle by estimating the spatial frequency to each side of the tunnel
## using the visual angles calculated in calc_vis_angle.
## This eliminates the assumption that the bird's gaze is centered on a stimulus
## cycle.


calc_s_freq <- function(obj_name,
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
  vertex_angle <- deg_2_rad(vertex_angle)

  ## duplicate object for simplify = TRUE
  obj_simplify <- obj_name

  ## Introduce variables for height_2_vertex and height_2_screen
  obj_name$height_2_vertex <- gnd_plane + obj_name$position_height
  obj_name$height_2_screen <- obj_name$height_2_vertex -
    (abs(obj_name$position_width) / tan(vertex_angle))


  ## Introduce variables for width_2_screen on positive and negative sides
  ## of the tunnel.
  ## width_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  obj_name$width_2_screen_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$height_2_screen * tan(vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE

  obj_name$width_2_screen_neg <-
    ifelse(obj_name$position_width < 0, # if in negative side of tunnel
           obj_name$height_2_screen * tan(vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE


  ## When the bird is outside the boundaries created by orthogonal planes to
  ## each screen, erroneous visual angles are calculated based on a
  ## minimum distance to either screen.
  ## Therefore min_dist values need to be adjusted according to position_width

  ## create variable of boundary values for each observation
  obj_name$bound_pos <- obj_name$height_2_vertex * tan(deg_2_rad(90) -
                                                         vertex_angle)
  obj_name$bound_neg <- obj_name$height_2_vertex * -tan(deg_2_rad(90) -
                                                          vertex_angle)


  ## Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen.
  obj_name$min_dist_pos <-
    ifelse(obj_name$position_width <= 0 &
             obj_name$position_width <= obj_name$bound_neg,
           # if position_width is negative and less than the boundary value
           sqrt(obj_name$height_2_vertex^2 + obj_name$position_width^2),
           # return distance to vertex
           obj_name$width_2_screen_pos * sin(deg_2_rad(90)- vertex_angle))
           # return minimum distance to positive screen

  obj_name$min_dist_neg <-
    ifelse(obj_name$position_width >= 0 &
             obj_name$position_width >= obj_name$bound_pos,
           # if position_width is positive and greater than the boundary value
           sqrt(obj_name$height_2_vertex^2 + obj_name$position_width^2),
           # return distance to vertex
           obj_name$width_2_screen_neg * sin(deg_2_rad(90) - vertex_angle))
           # return minimum distance to negative screen


  ## Calculate distance along plane of screen equal to 1˚ of visual angle.
  deg_dist_pos <- 2 * obj_name$min_dist_pos * tan(deg_2_rad(1))
  deg_dist_neg <- 2 * obj_name$min_dist_neg * tan(deg_2_rad(1))

  ## Calculate spatial frequency as number of cycles of stimulus per 1˚ of
  ## visual angle.
  obj_name$s_freq_pos <- deg_dist_pos / stim_param_pos
  obj_name$s_freq_neg <- deg_dist_neg / stim_param_neg


  ## Create simple data frame by adding min_dist and spatial frequencies
  obj_simplify$min_dist_pos <- obj_name$min_dist_pos
  obj_simplify$min_dist_neg <- obj_name$min_dist_neg
  obj_simplify$s_freq_pos <- obj_name$s_freq_pos
  obj_simplify$s_freq_neg <- obj_name$s_freq_neg

  ## return simple or complete data table based on simplify argument
  if(simplify_output == TRUE){
    return(obj_simplify)
  } else {
    return(obj_name)
  }
}

