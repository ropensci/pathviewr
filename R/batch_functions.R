## Part of the pathviewR package
## Last updated: 2020-07-12 VBB


################################# import_batch #################################
## Strictly importing, no further cleaning
##

## BAREBONES DRAFT OF ROXYGEN, NEEDS FURTHER DETAIL
#' Batch import of files for either Motive or Flydra (but not a mix of both)
#'
#' @param file_path_list A list of file paths
#' @param import_method Either "flydra" or "motive"
#' @param file_id Optional
#' @param subject_name For Flydra, the assigned subject name
#' @param frame_rate For Flydra, the assigned frame rate
#' @param simplify_marker_naming default TRUE; for Motive, whether marker naming
#' should be simplified
#' @param import_messaging default FALSE; should this function report each time
#' a file has been imported?
#' @param ... Additional arguments (may remove this if needed)
#'
#' @return
#' @family data import functions
#' @family batch functions
#' @export

import_batch <- function(file_path_list,
                         import_method = c("flydra", "motive"),
                         file_id = NA,
                         subject_name = NULL,
                         frame_rate = NULL,
                         simplify_marker_naming = TRUE,
                         import_messaging = FALSE,
                         ...){

  # if ("list" %in% class(file_path_list)){
  #   stop("file_path_list must be a list of file locations")
  # }

  imp_meth <- match.arg(import_method)

  ## Object to be filled
  obj_list <- NULL

  if (imp_meth == "flydra"){
    for (i in 1:length(file_path_list)){
      obj_list[[i]] <- read_flydra_data(
        mat_file = file_path_list[i],
        subject_name = subject_name[i],
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

## BAREBONES DRAFT OF ROXYGEN, NEEDS FURTHER DETAIL
#' For a list of viewr files, run through the pipeline (from relabel axes
#' up through get full trajectories, as desired) via clean_viewr()
#'
#' @param obj_list A list of viewr files (i.e. a list of tibbles)
#' @param ... Arguments to be passed in that specify how this function should
#' clean files.
#'
#' @return
#' @family batch functions
#' @export

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

## BAREBONES DRAFT OF ROXYGEN, NEEDS FURTHER DETAIL
#' Like clean viewr batch, but with import as the first step too
#'
#' @param file_path_list A list of file paths
#' @param import_method Either "flydra" or "motive"
#' @param file_id Optional
#' @param subject_name For Flydra, the subject name applied to all files
#' @param frame_rate For Flydra, the frame rate applied to all files
#' simplified?
#' @param import_messaging Should this function report each time a file has been
#' processed?
#' @param ... Additional arguments to specify how data should be cleaned.
#'
#' @return
#' @family data import functions
#' @family batch functions
#' @export

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
        subject_name = subject_name[i][i],
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

## BAREBONES DRAFT OF ROXYGEN, NEEDS FURTHER DETAIL
#' Combine multiple viewr objects into a single viwer object
#'
#' @param obj_list A list of viewr objects
#'
#' @return
#' @family batch functions
#' @export

bind_viewr_objects <- function(obj_list) {

  if (!"list" %in% class(obj_list)){
    stop("obj_list must be a list of viewr objects")
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
