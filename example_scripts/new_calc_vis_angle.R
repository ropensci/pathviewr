## This version of the function has a few less steps because some work as been
## offloaded to insert_treatments().


calc_s_freq_1 <- function(obj_name,
                        simplify_output = FALSE){

  ## Check that it's a viewr object
  if (!any(attr(obj_name, "pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_full_trajectories has been run
  if (!any(attr(obj_name, "pathviewR_steps") == "treatments_added")){
    stop("Run insert_treatments() prior to use.")
  }

  ## duplicate object for simplify = TRUE
  obj_simplify <- obj_name

  ## Introduce variables for height_2_vertex and height_2_screen
  obj_name$height_2_vertex <- abs(obj_name$vertex_height) + obj_name$position_height
  obj_name$height_2_screen <- obj_name$height_2_vertex -
    (abs(obj_name$position_width) / tan(obj_name$vertex_angle))
  ## Same but pulling from attributes
  #obj_name$height_2_screen <- (attr(obj_name, "vertex_height") -
  #                               (abs(obj_name$position_width) /
  #                                  tan(attr(obj_name, "vertex_angle"))))


  ## Introduce variables for width_2_screen on positive and negative sides
  ## of the tunnel.
  ## width_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  obj_name$width_2_screen_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$height_2_screen * tan(obj_name$vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(obj_name$vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE

  obj_name$width_2_screen_neg <-
    ifelse(obj_name$position_width < 0, # if in negative side of tunnel
           obj_name$height_2_screen * tan(obj_name$vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(obj_name$vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE


  ## When the bird is outside the boundaries created by orthogonal planes to
  ## each screen, erroneous visual angles are calculated based on a
  ## minimum distance to either screen.
  ## Therefore min_dist values need to be adjusted according to position_width

  ## create variable of boundary values for each observation
  obj_name$bound_pos <- obj_name$height_2_vertex * tan(deg_2_rad(90) -
                                                         obj_name$vertex_angle)
  obj_name$bound_neg <- obj_name$height_2_vertex * -tan(deg_2_rad(90) -
                                                          obj_name$vertex_angle)


  ## Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen.
  obj_name$min_dist_pos <-
    ifelse(obj_name$position_width <= 0 &
             obj_name$position_width <= obj_name$bound_neg,
           # if position_width is negative and less than the boundary value
           sqrt(obj_name$height_2_vertex^2 + obj_name$position_width^2),
           # return distance to vertex
           obj_name$width_2_screen_pos * sin(deg_2_rad(90)-
                                               obj_name$vertex_angle))
           # return minimum distance to positive screen

  obj_name$min_dist_neg <-
    ifelse(obj_name$position_width >= 0 &
             obj_name$position_width >= obj_name$bound_pos,
           # if position_width is positive and greater than the boundary value
           sqrt(obj_name$height_2_vertex^2 + obj_name$position_width^2),
           # return distance to vertex
           obj_name$width_2_screen_neg * sin(deg_2_rad(90) -
                                               obj_name$vertex_angle))
           # return minimum distance to negative screen


  ## Calculate distance along plane of screen equal to 1˚ of visual angle.
  deg_dist_pos <- 2 * obj_name$min_dist_pos * tan(deg_2_rad(1))
  deg_dist_neg <- 2 * obj_name$min_dist_neg * tan(deg_2_rad(1))

  ## Calculate spatial frequency as number of cycles of stimulus per 1˚ of
  ## visual angle.
  obj_name$s_freq_pos <- deg_dist_pos / obj_name$stim_param_pos
  obj_name$s_freq_neg <- deg_dist_neg / obj_name$stim_param_neg


  ## Create simple data frame by adding min_dist and spatial frequencies
  obj_simplify$min_dist_pos <- obj_name$min_dist_pos
  obj_simplify$min_dist_neg <- obj_name$min_dist_neg
  obj_simplify$s_freq_pos <- obj_name$s_freq_pos
  obj_simplify$s_freq_neg <- obj_name$s_freq_neg

  ## Leave note that spatial frequencies were calculated on dataset
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                                      "frequencies_calculated")

  ## return simple or complete data table based on simplify argument
  if(simplify_output == TRUE){
    return(obj_simplify)
  } else {
    return(obj_name)
  }
}

a <- calc_s_freq(t,
                 0.3855, 0.1, 0.1)

b <- calc_s_freq_1(t)


ggplot(a, aes(x = position_width, y = position_height)) +
  geom_point(aes(color = s_freq_pos))

ggplot(b, aes(x = position_width, y = position_height)) +
  geom_point(aes(color = s_freq_pos))

View(a)
View(b)
