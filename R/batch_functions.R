


################################# import_batch #################################
## Strictly importing, no further cleaning
##

import_batch <- function(file_path_list,
                         import_method = c("flydra", "motive"),
                         file_id = NA,
                         subject_name = NULL,
                         frame_rate = NULL,
                         simplify_marker_naming = TRUE,
                         import_messaging = FALSE,
                         ...){

  if ("list" %in% class(file_path_list)){
    stop("file_path_list must be a list of file locations")
  }

  imp_meth <- match.arg(import_method)

  ## Object to be filled
  obj_list <- NULL

  if (imp_meth == "flydra"){
    for (i in 1:length(file_path_list)){
      obj_list[[i]] <- read_flydra_data(
        mat_file = file_path_list[i],
        subject_name = treatz$subject[i],
        frame_rate = 100)
      if (import_messaging == TRUE)
        message("File ", i, " imported.")
    }
  }

  if (imp_meth == "motive"){
    for (i in 1:length(file_path_list)){
      obj_list[[i]] <- read_motive_csv(
        file_name = file_path_list[i],
        simplify_marker_naming = simplify_marker_naming)
      if (import_messaging == TRUE)
        message("File ", i, " imported.")
    }
  }

  ## Export
  return(obj_list)
}


################################# clean_viewr_batch #############################
## for a list of viewr files, run through the pipeline (from relabel axes
## up through get full trajectories, as desired) via clean_viewr()

clean_viewr_batch <- function(obj_list,
                              ...) {

  if (!"list" %in% class(obj_list)){
    stop("obj_list must be a list of file locations")
  }

  cleaned_list <- lapply(obj_list, FUN = clean_viewr, ...)

  ## Export
  return(cleaned_list)
}


############################ import_and_clean_batch ############################
## Like clean viewr batch, but with import as the first step too
##

import_and_clean_batch <- function(file_path_list,
                                   import_method = c("flydra", "motive"),
                                   file_id = NA,
                                   subject_name = NULL,
                                   frame_rate = NULL,
                                   simplify_marker_naming = TRUE,
                                   import_messaging = FALSE,
                                   ...){
  ## Revise this:
  # if (!"list" %in% class(file_path_list)){
  #   stop("file_path_list must be a list of file locations")
  # }

  imp_meth <- match.arg(import_method)

  ## Object to be filled
  obj_list <- NULL

  if (imp_meth == "flydra"){
    for (i in 1:length(file_path_list)){
      obj_list[[i]] <- read_flydra_data(
        mat_file = file_path_list[i],
        subject_name = treatz$subject[i],
        frame_rate = 100)
      if (import_messaging == TRUE)
        message("File ", i, " imported.")
    }
  }

  if (imp_meth == "motive"){
    for (i in 1:length(file_path_list)){
      obj_list[[i]] <- read_motive_csv(
        file_name = file_path_list[i],
        simplify_marker_naming = simplify_marker_naming)
      if (import_messaging == TRUE)
        message("File ", i, " imported.")
    }
  }

  ## Now clean
  obj_cleaned <- clean_viewr_batch(obj_list, ...)

  ## Export
  return(obj_cleaned)
}


############################### bind_viewr_objects #############################
## combine multiple viewr objects into a single viwer object
##

bind_viewr_objects <- function(obj_list) {

  if (!"list" %in% class(obj_list)){
    stop("obj_list must be a list of file locations")
  }

  ## THIS FUNCTION LIKELY NEEDS TO BE EDITED TO ACCOMODATE ATTRIBUTES FROM
  ## EACH OBJECT. Not sure what exactly should be retained from each and how
  ## it all needs to be strung together yet...

  ## Combine the objects
  bound_obj <- dplyr::bind_rows(obj_list)

  ## Attributes
  attr(bound_obj, "pathviewR_steps") <- c("viewr", "bound_viewr_objects")

  ## Export
  return(bound_obj)

}
