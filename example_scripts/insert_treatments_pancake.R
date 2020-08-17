
### With the insert_treatments() function, users can add tunnel set up details
### and visual stimulus information for each treatment. Currently, it only
### accepts a single treatment but this will be expanded in the future.
### Tunnel and stimulus info is also stored in the data object's attributes.

### This function must be run after get_full_trajectories

insert_treatments <- function(obj_name,
                              vertex_height,
                              vertex_angle,
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
  ## NOTE: make sure this doesn't eff up other functions that depend on the
  ## position of certain variables remaining constant
  obj_name <- tibble::add_column(obj_name, .before = "frame",
                                 vertex_height = vertex_height,
                                 vertex_angle = deg_2_rad(vertex_angle),
                                 treatment = treatment)

  ## Add arguments into metadata......surewhynot
  attr(obj_name, "vertex_height") <- vertex_height
  attr(obj_name, "vertex_angle") <- vertex_angle
  attr(obj_name, "treatment") <- treatment


  ## Create empty stim_param variables
  obj_name$stim_param_pos <- vector(mode = "numeric", length = nrow(obj_name))
  obj_name$stim_param_neg <- vector(mode = "numeric", length = nrow(obj_name))


  ## Add stimulus parameters based on treatment name (will update with actual
  ## values once I have them)
  # stim_param_pos
  for (i in 1:nrow(obj_name)){
    if (obj_name$treatment[[i]] == "latA"){
      obj_name$stim_param_pos[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latB"){
      obj_name$stim_param_pos[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latC"){
      obj_name$stim_param_pos[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latD"){
      obj_name$stim_param_pos[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latE"){
      obj_name$stim_param_pos[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latF"){
      obj_name$stim_param_pos[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latG"){
      obj_name$stim_param_pos[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latH"){
      obj_name$stim_param_pos[[i]] <- 0.1
    }
  }

  # stim_param_neg (again I will update with real values)
  for (i in 1:nrow(obj_name)){
    if (obj_name$treatment[[i]] == "latA"){
      obj_name$stim_param_neg[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latB"){
      obj_name$stim_param_neg[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latC"){
      obj_name$stim_param_neg[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latD"){
      obj_name$stim_param_neg[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latE"){
      obj_name$stim_param_neg[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latF"){
      obj_name$stim_param_neg[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latG"){
      obj_name$stim_param_neg[[i]] <- 0.1
    } else if (obj_name$treatment[[i]] == "latH"){
      obj_name$stim_param_neg[[i]] <- 0.1
    }
  }

  ## Leave note that treatments were added
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                                        "treatments_added")
  return(obj_name)
}




#### testing zone

t <- insert_treatments(jul_29_full,
                       vertex_height = -0.3855,
                       vertex_angle = 45,
                       treatment = "latD")

View(t)

attributes(test)

jul_29_full


