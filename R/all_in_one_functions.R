############################### all_in_one_function ############################
## Use all of the preceding functions to construct an all-in-one function for
## ease of use.
##
## NOTE 2020-06-17 We will need to accomodate for future import functions such
## as read_flydra_mat() (or whatever it will be named). Ideally this will be
## autodetected based on file type and switched internally.
##
## NOTE 2020-06-25 This has been massively re-written to accomodate flexiblity
## in user choices. By default, it runs through the pipeline we had before:
## read -> relabel -> gather -> trim -> rotate -> select -> separate -> get_full
## But, by adding TRUE/FALSE statements, a user may opt to skip steps as needed.
## Also, there is choice of tunnel standardization.

import_and_clean_viewr <- function(file_name,
                                   file_id = NA,
                                   relabel_viewr_axes = TRUE,
                                   gather_tunnel_data = TRUE,
                                   trim_tunnel_outliers = TRUE,
                                   standardization_option = "rotate_tunnel",
                                   select_x_percent = TRUE,
                                   separate_trajectories = TRUE,
                                   get_full_trajectories = TRUE,
                                   ...){

  ## Import checks
  if (missing(file_name))
    stop("A file_name is required")
  if (!file.exists(file_name))
    stop(paste0("File ", file_name, " not found!"))

  ## ADD CHECK HERE FOR FILETYPE (CSV OR MAT) AND THEN HANDLE ACCORDINGLY

  ## Check that any arguments supplied are valid; return a warning if not
  valid_args <- c(
    ## read_motive_csv()
    "file_name", "file_id",
    ## relabel_viewr_axes()
    "tunnel_length", "tunnel_width", "tunnel_height", "real",
    ## trim_tunnel_outliers()
    "lengths_min", "lengths_max",
    "widths_min", "widths_max",
    "heights_min", "heights_max",
    ## rotate_tunnel()
    "all_heights_min", "all_heights_max",
    ## standardize_tunnel()
    "landmark_one", "landmark_two",
    ## perch 1 = left (near length = 0); perch 2 = right
    "perch1_len_min", "perch1_len_max",
    "perch2_len_min", "perch2_len_max",
    "perch1_wid_min", "perch1_wid_max",
    "perch2_wid_min", "perch2_wid_max",
    ## select_x_percent()
    "desired_percent",
    ## separate_trajectories()
    "max_frame_gap",
    ## get_full_trajectories()
    "span"
  )
  arg_names <- names(list(...))
  if (!all(arg_names %in% valid_args)) {
    warning("One or more provided arguments does not match known arguments.
            \nThese will not be used.")
  }

  ## Check standardization choice
  valid_stands <- c("rotate_tunnel",
                    "standardize_tunnel",
                    "center_tunnel_data",
                    "none")
  if (!all(standardization_option %in% valid_stands)) {
    warning("standardization_option must be one of the following:
\"rotate_tunnel\", \"standardize_tunnel\", \"center_tunnel_data\", or \"none\"")
  }

  ## Start the pipeline
  obj <-
    file_name %>%
    read_motive_csv(...)

  if (relabel_viewr_axes == TRUE) {
    obj <-
      obj %>%
      relabel_viewr_axes(...)
  } else {
    obj <- obj
  }

  if (gather_tunnel_data == TRUE) {
    obj <-
      obj %>%
      gather_tunnel_data(...)
  } else {
    obj <- obj
  }

  if (trim_tunnel_outliers == TRUE) {
    obj <-
      obj %>%
      trim_tunnel_outliers(...)
  } else {
    obj <- obj
  }

  if (standardization_option == "rotate_tunnel"){
    obj <-
      obj %>%
      rotate_tunnel(...)
  }

  if (standardization_option == "standardize_tunnel"){
    obj <-
      obj %>%
      standardize_tunnel(...)
  }

  if (standardization_option == "center_tunnel_data"){
    obj <-
      obj %>%
      rotate_tunnel(...) ## REPLACE WHEN CENTER_TUNNEL_DATA IS READY!
  }

  if (standardization_option == "none"){
    obj <- obj
  }

  if (select_x_percent == TRUE) {
    obj <-
      obj %>%
      select_x_percent(...)
  } else {
    obj <- obj
  }

  if (separate_trajectories == TRUE) {
    obj <-
      obj %>%
      separate_trajectories(...)
  } else {
    obj <- obj
  }

  if (get_full_trajectories == TRUE) {
    obj <-
      obj %>%
      get_full_trajectories(...)
  } else {
    obj <- obj
  }

  ## Export
  return(obj)
}
