#################        calc_vis_angle_1        ###################

## Assuming fixed horizontal gaze, calculate the visual angle subtended by the 
## length of a stimulus feature on either screen and the eye of the bird. 
## Positive and negative sides of tunnel reflect positive and negative 
## values of "Position_widths"
## 
###########  NOTE THIS FUNCTION ONLY WORKS WITH VERTEX ANGLE IS 45˚  ###########

calc_vis_angle_1 <- function(obj_name,
                             gnd_plane,
                             stim_param){
 
  
  ## Part 1. Introduce variables for height_2_vertex and height_2_screen 
  
  obj_name <- obj_name %>% 
     mutate(height_2_vertex = gnd_plane + Position_heights,
            height_2_screen = height_2_vertex - (abs(Position_widths)))
        
      
   ## Part 2. Introduce variables for width_2_screen on positive and negative sides
   ## of the tunnel using ifelse() which combines a conditional and a for loop.
   ## width_2_screen refers to the horizontal distance between the bird and
   ## either screen.
  
 obj_name$width_2_screen_pos <- 
   ifelse(obj_name$Position_widths >= 0, 
            # if bird is in positive side of tunnel
          obj_name$height_2_screen, 
            # TRUE = height_2_screen
          obj_name$height_2_screen + 2 * abs(obj_name$Position_widths)) 
            # FALSE = height_2_screen + (2 * Position_width)
   
 obj_name$width_2_screen_neg <- 
   ifelse(obj_name$Position_widths < 0, 
            # if bird is in negative side of tunnel
          obj_name$height_2_screen, 
            # TRUE = height_2_screen
          obj_name$height_2_screen + 2 * abs(obj_name$Position_widths)) 
            # FALSE = height_2_screen + (2 * Position_width)  

    
    ## Part 3. Calculate visual angles (radians and degrees) using distance to 
    ## positive and negative screens. Add these variables into the dataframe. 
    
 obj_name <- obj_name %>% 
   mutate(vis_angle_pos_rad =  2*atan(stim_param/(2*width_2_screen_pos)),
          # radians
          vis_angle_neg_rad =  2*atan(stim_param/(2*width_2_screen_neg)),
          # radians
          vis_angle_pos_deg =  vis_angle_pos_rad * (180 / pi),
          # convert to deg
          vis_angle_neg_deg =  vis_angle_neg_rad * (180 / pi))
          # convert to deg

  return(obj_name) 
 }


#################        calc_vis_angle_1.1        ###################

## Same as above but without adding all internal variables to the dataframe

calc_vis_angle_1.1 <- function(obj_name,
                             gnd_plane,
                             stim_param){
  
  
  ## Part 1. Introduce columns for height_2_vertex and height_2_screen 
  
  height_2_vertex <- gnd_plane + obj_name$Position_heights
  height_2_screen <- height_2_vertex - (abs(obj_name$Position_widths))
  
  
  ## Part 2. Introduce variables width_2_screen on positive and negative sides
  ## of the tunnel using ifelse() which combines a conditional and a for loop
  
  width_2_screen_pos <- 
    ifelse(obj_name$Position_widths >= 0, 
           # if bird is in positive side of tunnel
           height_2_screen, 
           # TRUE = height_2_screen
           height_2_screen + 2 * abs(obj_name$Position_widths)) 
           # FALSE = height_2_screen + (2 * Position_width)
  
  width_2_screen_neg <- 
    ifelse(obj_name$Position_widths < 0, 
           # if bird is in negative side of tunnel
           height_2_screen, 
           # TRUE = height_2_screen
           height_2_screen + 2 * abs(obj_name$Position_widths)) 
           # FALSE = height_2_screen + (2 * Position_width)  
  
  
  ## Part 3. Calculate visual angles (radians and degrees) using distance to 
  ## positive and negative screens. Introduce columns for these degree variables
  
  vis_angle_pos_rad <- 2*atan(stim_param/(2*width_2_screen_pos)) # radians
  vis_angle_neg_rad <- 2*atan(stim_param/(2*width_2_screen_neg)) # radians
  
  obj_name <- obj_name %>% 
    mutate(vis_angle_pos_deg =  vis_angle_pos_rad * (180 / pi), # convert to deg
           vis_angle_neg_deg =  vis_angle_neg_rad * (180 / pi)) # convert to deg
  
  return(obj_name) 
}

