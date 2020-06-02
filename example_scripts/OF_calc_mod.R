#################        calc_vis_angle_mod       ###################

## Assuming gaze is fixed at the closest point of the screen on either side of
## the tunnel (45˚ down from horizontal), calculate the visual angle subtended 
## by the length of a stimulus feature on either side of the screen and the eye 
## of the bird. Positive and negative sides of the tunnel reflect positive and 
## negative values of "Position_widths"

calc_vis_angle_mod <- function(obj_name,
                               vertex_angle,
                               gnd_plane,
                               stim_param){
  
  
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
    obj_name$width_2_screen_pos * sin(90 - vertex_angle) 
    # min_dist to positive screen
  
  obj_name$min_dist_neg <- 
    obj_name$width_2_screen_neg * sin(90 - vertex_angle) 
    # min_dist to negative screen
  
  
  ## Part 5. Calculate visual angles (radians and degrees) using distance to 
  ## positive and negative screens. Add these variables into the dataframe. 
  obj_name <- obj_name %>% 
    mutate(vis_angle_pos_rad =  2*atan(stim_param/(2*obj_name$min_dist_pos)),
           # radians
           vis_angle_neg_rad =  2*atan(stim_param/(2*obj_name$min_dist_neg)),
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
                                 vertex_angle,
                                 gnd_plane,
                                 stim_param){
  
  
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
  min_dist_pos <- width_2_screen_pos * sin(90 - vertex_angle) 
  # min_dist to positive screen
  
  min_dist_neg <- width_2_screen_neg * sin(90 - vertex_angle) 
  # min_dist to negative screen
  
  
  ## Part 5. Calculate visual angles (radians and degrees) using distance to 
  ## positive and negative screens. Add the degrees variables into the dataframe. 
  vis_angle_pos_rad <- 2 * atan(stim_param / (2 * min_dist_pos)) # radians
  vis_angle_neg_rad <- 2 * atan(stim_param / (2 * min_dist_neg)) # radians
  
  obj_name <- obj_name %>% 
    mutate(vis_angle_pos_deg =  vis_angle_pos_rad * (180 / pi), # convert to deg
           vis_angle_neg_deg =  vis_angle_neg_rad * (180 / pi)) # convert to deg
  
  return(obj_name) 
}




