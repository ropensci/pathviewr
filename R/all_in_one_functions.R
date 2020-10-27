## Part of the pathviewR package
## Last updated: 2020-09-17 VBB


################################# clean_viewr ##################################

#' All-in-one function to clean imported objects
#'
#' For an imported viewr object, run through the cleaning pipeline as desired
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param relabel_viewr_axes default TRUE, should axes be relabeled?
#' @param gather_tunnel_data default TRUE, should tunnel data be gathered?
#' @param trim_tunnel_outliers default TRUE, outliers be trimmed?
#' @param standardization_option default "rotate_tunnel"; which standardization
#'   option should be used? See Details for more.
#' @param get_velocity default TRUE, should velocity be computed?
#' @param select_x_percent default TRUE, should a region of interest be
#'   selected?
#' @param rename_viewr_characters default FALSE, should subjects be renamed?
#' @param separate_trajectories default TRUE, should trajectories be defined?
#' @param get_full_trajectories default TRUE, should only full trajectories be
#'   retained?
#' @param fill_traj_gaps default FALSE, should gaps in trajectories be filled?
#' @param ... Additional arguments passed to any of the corresponding functions
#'
#' @details Each argument corresponds to a standalone function in
#'   \code{pathviewR}. E.g. the parameter \code{relabel_viewr_axes} allows a
#'   user to choose whether \code{pathviewR::relabel_viewr_axes()} is run
#'   internally. Should the user desire to use any non-default parameter values
#'   for any functions included here, they should be supplied to this function
#'   as additional arguments formatted exactly as they would appear in their
#'   corresponding function(s). E.g. if the "autodetect" feature in
#'   \code{pathviewR::separate_trajectories()} is desired, add an argument
#'   \code{max_frame_gap = "autodetect"} to the arguments supplied to this
#'   function.
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) that has passed
#'   through several \code{pathviewR} functions as desired by the user,
#'   resulting in data that have been cleaned and ready for analyses.
#'
#' @author Vikram B. Baliga
#'
#' @family all in one functions
#'
#' @export
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' motive_full <-
#'   motive_data %>%
#'   clean_viewr(desired_percent = 50,
#'               max_frame_gap = "autodetect",
#'               span = 0.95)
#'
#' ## Alternatively, used the import_and_clean_viewr()
#' ## function to combine these steps
#' motive_import_and_clean <-
#'   import_and_clean_viewr(
#'     file_name = system.file("extdata", "pathviewR_motive_example_data.csv",
#'                             package = 'pathviewR'),
#'     desired_percent = 50,
#'     max_frame_gap = "autodetect",
#'     span = 0.95
#'   )

clean_viewr <- function(obj_name,
                        relabel_viewr_axes = TRUE,
                        gather_tunnel_data = TRUE,
                        trim_tunnel_outliers = TRUE,
                        standardization_option = "rotate_tunnel",
                        get_velocity = TRUE,
                        select_x_percent = TRUE,
                        rename_viewr_characters = FALSE,
                        separate_trajectories = TRUE,
                        get_full_trajectories = TRUE,
                        fill_traj_gaps = FALSE,
                        ...) {

  ## Check that any arguments supplied are valid; return a warning if not
  ## relabel_viewr_axes()
  relabel_args <- c("tunnel_length", "tunnel_width", "tunnel_height", "real")
  ## gather_tunnel_data
  gather_args <- c("NA_drop")
  ## trim_tunnel_outliers()
  trim_args <- c("lengths_min", "lengths_max",
                 "widths_min", "widths_max",
                 "heights_min", "heights_max")
  ## rotate_tunnel()
  rotate_args <- c("all_heights_min", "all_heights_max")
  ## standardize_tunnel()
  standardize_args <- c("landmark_one", "landmark_two",
                        ## perch 1 = left (near length = 0); perch 2 = right
                        "perch1_len_min", "perch1_len_max",
                        "perch2_len_min", "perch2_len_max",
                        "perch1_wid_min", "perch1_wid_max",
                        "perch2_wid_min", "perch2_wid_max")
  ## redefine_tunnel_center()
  center_args <- c("axes",
                   "length_method", "width_method", "height_method",
                   "length_zero", "width_zero", "height_zero")
  ## get_velocity()
  velocity_args <- c("time_col",
                     "length_col", "width_col", "height_col",
                     "add_to_viewr",
                     "velocity_min", "velocity_max")
  ## select_x_percent()
  select_args <- c("desired_percent")
  ## rename_viewr_characters()
  rename_args <- c("target_column", "pattern", "replacement")
  ## separate_trajectories()
  separate_args <- c("max_frame_gap", "frame_rate_proportion",
                     "frame_gap_messaging", "frame_gap_plotting")
  ## get_full_trajectories()
  get_full_traj_args <- c("span")
  ## fill_traj_gaps()
  fill_traj_gaps_args <- c("loess_degree", "loess_criterion", "loess_family",
                           "loess_user_span")

  valid_args <- c(relabel_args, trim_args,
                  rotate_args, standardize_args, center_args,
                  velocity_args,
                  select_args, rename_args, separate_args, get_full_traj_args,
                  fill_traj_gaps_args
  )

  arg_names <- names(list(...))
  ## Check for any unrecognized arguments and message() about them
  unrecog_params <- setdiff(arg_names, valid_args)
  if (length(unrecog_params)
  )
    message("Unrecognized arguments: ", paste(unrecog_params, collapse = ", "),
            "\nAny arguments that are unrecognized will not be used.")

  ## Check standardization choice
  valid_stands <- c("rotate_tunnel",
                    "standardize_tunnel",
                    "redefine_tunnel_center",
                    "none")
  if (!all(standardization_option %in% valid_stands)) {
    stop(
      "standardization_option must be one of the following:
\"rotate_tunnel\", \"standardize_tunnel\",
\"redefine_tunnel_center\", or \"none\""
    )
  }

  ## Start the pipeline
  obj <-
    obj_name

  if (relabel_viewr_axes == TRUE) {
    obj <-
      obj %>%
      relabel_viewr_axes(...)
  } else {
    if (any(arg_names %in% relabel_args)) {
      stop(
        "At least one argument for relabel_viewr_axes() was supplied,
but relabel_viewr_axes was set to FALSE.
Please resolve by either setting relabel_viewr_axes to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (gather_tunnel_data == TRUE) {
    obj <-
      obj %>%
      gather_tunnel_data(...)
  } else {
    if (any(arg_names %in% gather_args)) {
      stop(
        "At least one argument for gather_tunnel_data() was supplied,
but gather_tunnel_data was set to FALSE.
Please resolve by either setting gather_tunnel_data to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (trim_tunnel_outliers == TRUE) {
    obj <-
      obj %>%
      trim_tunnel_outliers(...)
  } else {
    if (any(arg_names %in% trim_args)) {
      stop(
        "At least one argument for trim_tunnel_outliers() was supplied,
but trim_tunnel_outliers was set to FALSE.
Please resolve by either setting trim_tunnel_outliers to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (standardization_option == "rotate_tunnel") {
    obj <-
      obj %>%
      rotate_tunnel(...)
  }

  if (standardization_option == "standardize_tunnel") {
    obj <-
      obj %>%
      standardize_tunnel(...)
  }

  if (standardization_option == "redefine_tunnel_center") {
    obj <-
      obj %>%
      redefine_tunnel_center(...)
  }

  if (standardization_option == "none") {
    obj <- obj
  }

  if (get_velocity == TRUE) {
    obj <-
      obj %>%
      get_velocity(...)
  } else {
    if (any(arg_names %in% velocity_args)) {
      stop(
        "At least one argument for get_velocity() was supplied,
but get_velocity was set to FALSE.
Please resolve by either setting get_velocity to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (select_x_percent == TRUE) {
    obj <-
      obj %>%
      select_x_percent(...)
  } else {
    if (any(arg_names %in% select_args)) {
      stop(
        "At least one argument for select_x_percent() was supplied,
but select_x_percent was set to FALSE.
Please resolve by either setting select_x_percent to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (rename_viewr_characters == TRUE) {
    params <- list(...)
    obj <-
      obj %>%
      rename_viewr_characters(
        target_column = params$target_column,
        pattern = params$pattern,
        replacement = params$replacement
      )
  } else {
    if (any(arg_names %in% rename_args)) {
      stop(
        "At least one argument for rename_viewr_characters() was supplied,
but rename_viewr_characters was set to FALSE.
Please resolve by either setting rename_viewr_characters to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (separate_trajectories == TRUE) {
    obj <-
      obj %>%
      separate_trajectories(...)
  } else {
    if (any(arg_names %in% separate_args)) {
      stop(
        "At least one argument for separate_trajectories() was supplied,
but separate_trajectories was set to FALSE.
Please resolve by either setting separate_trajectories to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (get_full_trajectories == TRUE) {
    obj <-
      obj %>%
      get_full_trajectories(...)
  } else {
    if (any(arg_names %in% get_full_traj_args)) {
      stop(
        "At least one argument for get_full_trajectories() was supplied,
but get_full_trajectories was set to FALSE.
Please resolve by either setting get_full_trajectories to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (fill_traj_gaps == TRUE) {
    params <- list(...)
    obj <-
      obj %>%
      fill_traj_gaps(
        loess_degree = params$loess_degree,
        loess_criterion = params$loess_criterion,
        loess_family = params$loess_family,
        loess_user_span = params$loess_user_span
      )
  } else {
    if (any(arg_names %in% fill_traj_gaps_args)) {
      stop(
        "At least one argument for fill_traj_gaps() was supplied,
but fill_traj_gaps was set to FALSE.
Please resolve by either setting fill_traj_gaps to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  ## Check for any unused arguments and message() about them
  params <- list(...)
  optional_param_names <- valid_args
  unused_params <- setdiff(names(params), optional_param_names)
  if (length(unused_params))
    message("Unused parameters: ", paste(unused_params, collapse = ", "))

  ## Export
  return(obj)

}


############################ import_and_clean_viewr ############################
## Use all of the preceding functions to construct an all-in-one function for
## ease of use.

#' Import + clean_viewr()
#'
#' Import a file and then, akin to \code{clean_viewr}, run through as many
#' cleaning steps as desired.
#'
#' @param file_name Target file
#' @param file_id Optional
#' @inheritParams clean_viewr
#' @param ... Additional arguments passed to the corresponding functions.
#'
#' @inherit clean_viewr return details examples
#'
#' @author Vikram B. Baliga
#'
#' @family all in one functions
#'
#' @export
#'
#' @inherit clean_viewr examples

import_and_clean_viewr <- function(file_name,
                                   file_id = NA,
                                   relabel_viewr_axes = TRUE,
                                   gather_tunnel_data = TRUE,
                                   trim_tunnel_outliers = TRUE,
                                   standardization_option = "rotate_tunnel",
                                   get_velocity = TRUE,
                                   select_x_percent = TRUE,
                                   rename_viewr_characters = FALSE,
                                   separate_trajectories = TRUE,
                                   get_full_trajectories = TRUE,
                                   fill_traj_gaps = FALSE,
                                   ...) {
  ## Import checks
  if (missing(file_name))
    stop("A file_name is required")
  if (!file.exists(file_name))
    stop(paste0("File ", file_name, " not found!"))

  ## ADD CHECK HERE FOR FILETYPE (CSV OR MAT) AND THEN HANDLE ACCORDINGLY

  ## Check that any arguments supplied are valid; return a warning if not
    ## read_motive_csv()
    read_args <- c("file_name", "file_id")
    ## relabel_viewr_axes()
    relabel_args <- c("tunnel_length", "tunnel_width", "tunnel_height", "real")
    ## gather_tunnel_data
    gather_args <- c("NA_drop")
    ## trim_tunnel_outliers()
    trim_args <- c("lengths_min", "lengths_max",
                   "widths_min", "widths_max",
                   "heights_min", "heights_max")
    ## rotate_tunnel()
    rotate_args <- c("all_heights_min", "all_heights_max")
    ## standardize_tunnel()
    standardize_args <- c("landmark_one", "landmark_two",
        ## perch 1 = left (near length = 0); perch 2 = right
                          "perch1_len_min", "perch1_len_max",
                          "perch2_len_min", "perch2_len_max",
                          "perch1_wid_min", "perch1_wid_max",
                          "perch2_wid_min", "perch2_wid_max")
    ## redefine_tunnel_center()
    center_args <- c("axes",
                     "length_method", "width_method", "height_method",
                     "length_zero", "width_zero", "height_zero")
    ## get_velocity()
    velocity_args <- c("time_col",
                       "length_col", "width_col", "height_col",
                       "add_to_viewr",
                       "velocity_min", "velocity_max")
    ## select_x_percent()
    select_args <- c("desired_percent")
    ## rename_viewr_characters()
    rename_args <- c("target_column", "pattern", "replacement")
    ## separate_trajectories()
    separate_args <- c("max_frame_gap", "frame_rate_proportion",
                       "frame_gap_messaging", "frame_gap_plotting")
    ## get_full_trajectories()
    get_full_traj_args <- c("span")
    ## fill_traj_gaps()
    fill_traj_gaps_args <- c("loess_degree", "loess_criterion", "loess_family",
                             "loess_user_span")

  valid_args <- c(read_args, relabel_args, trim_args,
                  rotate_args, standardize_args, center_args,
                  velocity_args,
                  select_args, rename_args, separate_args, get_full_traj_args,
                  fill_traj_gaps_args
                  )

  arg_names <- names(list(...))
  ## Check for any unrecognized arguments and message() about them
  unrecog_params <- setdiff(arg_names, valid_args)
  if (length(unrecog_params))
    message(
      "Unrecognized arguments: ",
      paste(unrecog_params, collapse = ", "),
      "\nAny arguments that are unrecognized will not be used."
    )


  ## Check standardization choice
  valid_stands <- c("rotate_tunnel",
                    "standardize_tunnel",
                    "redefine_tunnel_center",
                    "none")
  if (!all(standardization_option %in% valid_stands)) {


    stop(
      "standardization_option must be one of the following:
\"rotate_tunnel\", \"standardize_tunnel\",
\"redefine_tunnel_center\", or \"none\""
    )
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
    if (any(arg_names %in% relabel_args)) {
      stop(
"At least one argument for relabel_viewr_axes() was supplied,
but relabel_viewr_axes was set to FALSE.
Please resolve by either setting relabel_viewr_axes to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (gather_tunnel_data == TRUE) {
    obj <-
      obj %>%
      gather_tunnel_data(...)
  } else {
    if (any(arg_names %in% gather_args)) {
      stop(
"At least one argument for gather_tunnel_data() was supplied,
but gather_tunnel_data was set to FALSE.
Please resolve by either setting gather_tunnel_data to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (trim_tunnel_outliers == TRUE) {
    obj <-
      obj %>%
      trim_tunnel_outliers(...)
  } else {
    if (any(arg_names %in% trim_args)) {
      stop(
"At least one argument for trim_tunnel_outliers() was supplied,
but trim_tunnel_outliers was set to FALSE.
Please resolve by either setting trim_tunnel_outliers to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (standardization_option == "rotate_tunnel") {
    obj <-
      obj %>%
      rotate_tunnel(...)
  }

  if (standardization_option == "standardize_tunnel") {
    obj <-
      obj %>%
      standardize_tunnel(...)
  }

  if (standardization_option == "redefine_tunnel_center") {
    obj <-
      obj %>%
      redefine_tunnel_center(...)
  }

  if (standardization_option == "none") {
    obj <- obj
  }

  if (get_velocity == TRUE) {
    obj <-
      obj %>%
      get_velocity(...)
  } else {
    if (any(arg_names %in% velocity_args)) {
      stop(
"At least one argument for get_velocity() was supplied,
but get_velocity was set to FALSE.
Please resolve by either setting get_velocity to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (select_x_percent == TRUE) {
    obj <-
      obj %>%
      select_x_percent(...)
  } else {
    if (any(arg_names %in% select_args)) {
      stop(
"At least one argument for select_x_percent() was supplied,
but select_x_percent was set to FALSE.
Please resolve by either setting select_x_percent to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (rename_viewr_characters == TRUE) {
    params <- list(...)
    obj <-
      obj %>%
      rename_viewr_characters(
        target_column = params$target_column,
        pattern = params$pattern,
        replacement = params$replacement
      )
  } else {
    if (any(arg_names %in% rename_args)) {
      stop(
        "At least one argument for rename_viewr_characters() was supplied,
but rename_viewr_characters was set to FALSE.
Please resolve by either setting rename_viewr_characters to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (separate_trajectories == TRUE) {
    obj <-
      obj %>%
      separate_trajectories(...)
  } else {
    if (any(arg_names %in% separate_args)) {
      stop(
"At least one argument for separate_trajectories() was supplied,
but separate_trajectories was set to FALSE.
Please resolve by either setting separate_trajectories to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (get_full_trajectories == TRUE) {
    obj <-
      obj %>%
      get_full_trajectories(...)
  } else {
    if (any(arg_names %in% get_full_traj_args)) {
      stop(
"At least one argument for get_full_trajectories() was supplied,
but get_full_trajectories was set to FALSE.
Please resolve by either setting get_full_trajectories to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  if (fill_traj_gaps == TRUE) {
    params <- list(...)
    obj <-
      obj %>%
      fill_traj_gaps(
        loess_degree = params$loess_degree,
        loess_criterion = params$loess_criterion,
        loess_family = params$loess_family,
        loess_user_span = params$loess_user_span
      )
  } else {
    if (any(arg_names %in% fill_traj_gaps_args)) {
      stop(
        "At least one argument for fill_traj_gaps() was supplied,
but fill_traj_gaps was set to FALSE.
Please resolve by either setting fill_traj_gaps to TRUE
or by removing the extraneous argument(s)")
    }
    obj <- obj
  }

  ## Check for any unused arguments and message() about them
  params <- list(...)
  optional_param_names <- valid_args
  unused_params <- setdiff(names(params), optional_param_names)
  if (length(unused_params)
  )
    message("Unused parameters: ", paste(unused_params, collapse = ", "))

  ## Export
  return(obj)
}
