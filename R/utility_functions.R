## Part of the pathviewR package
## Last updated: 2020-09-05 VBB


################################# get_header_viewr #############################
#' Extract header info from imported viewr object
#'
#' A function to quickly return the information stored in the header of the
#' original data file imported via \code{pathviewR}'s \code{read_} functions.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' \code{pathviewR_steps}
#' @param ... Additional arguments that may be passed to other \code{pathviewR}
#' functions
#'
#' @return The value of the \code{header} attribute, or NULL if no exact match
#' is found and no or more than one partial match is found.
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @family metadata handling functions
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the Motive example data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' ## Now display the Header information
#' get_header_viewr(motive_data)


get_header_viewr <- function(obj_name,
                             ...) {
  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Get the header
  return(attr(obj_name,"header"))
}


############################### relabel_viewr_axes #############################

#' Relabel the dimensions as length, width, and height
#'
#' Axes are commonly labeled as "x", "y", and "z" in recording software yet
#' \code{pathviewR} functions require these to be labeled as "length", "width",
#' and "height". \code{relabel_viewr_axes()} is a function that takes a
#' \code{viewr} object and allows the user to rename its variables.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param tunnel_length The dimension that corresponds to tunnel length. Set to
#' \code{tunnel_length = "_z"} by default. Argument should contain a character
#' vector with a leading underscore (see Details)
#' @param tunnel_width The dimension that corresponds to tunnel width. Follows
#' the same conventions as \code{tunnel_length} and defaults to
#' \code{tunnel_length = "_x"}
#' @param tunnel_height The dimension that corresponds to tunnel height. Follows
#' the same conventions as \code{tunnel_length} and defaults to
#' \code{tunnel_length = "_y"}
#' @param real The dimension that corresponds to the "real" parameter in
#' quaternion notation (for data with "rotation" values). Follows the same
#' conventions as \code{tunnel_length} and defaults to \code{real = "_w"}
#' @param ... Additional arguments to be passed to \code{read_motive_csv()}.
#'
#' @details Each argument must have a leading underscore ("_") and each
#' argument must have an entry. E.g. tunnel_length = "_Y" will replace all
#' instances of _Y with _length in the names of variables.
#'
#' @return A tibble or data.frame with variables that have been renamed.
#'
#' @author Vikram B. Baliga
#' @export
#'
#' @family data cleaning functions
#'
#' @examples
#'
#' library(pathviewR)
#'
#' ## Import the Motive example data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' ## Names of variables are labeled with _x, _y, _z, which we'd like to rename
#' names(motive_data)
#'
#' ## Now use relabel_viewr_axes() to rename these variables using _length,
#' ## _width, and _height instead
#' motive_data_relabeled <-
#'   relabel_viewr_axes(motive_data,
#'                      tunnel_length = "_z",
#'                      tunnel_width = "_x",
#'                      tunnel_height = "_y",
#'                      real = "_w")
#'
#' ## See the result
#' names(motive_data_relabeled)


relabel_viewr_axes <- function(obj_name,
                               tunnel_length = "_z",
                               tunnel_width = "_x",
                               tunnel_height = "_y",
                               real = "_w",
                               ...){
  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Inputs should be character vectors
  if (!is.character(tunnel_length)) {
    stop("tunnel_length should be a character vector")
  }
  if (!is.character(tunnel_width)) {
    stop("tunnel_width should be a character vector")
  }
  if (!is.character(tunnel_height)) {
    stop("tunnel_height should be a character vector")
  }

  namez <- base::names(obj_name)
  # THE FOLLOWING STEP MUST COME BEFORE WIDTH RENAMING! This is because the
  # 'real' dimension in quaternion notation is denoted "w", which conflicts
  # with the "w" in width
  namez <- sub(real,"_real",namez)
  namez <- sub(tunnel_width,"_width",namez)
  namez <- sub(tunnel_length,"_length",namez)
  namez <- sub(tunnel_height,"_height",namez)

  ## Note for future selves: The above assumes that X, Y, and Z apply in the
  ## same way to rotations as they do for positions. I am not familiar enough
  ## with quaternion notation yet to say. Should e.g. rotation_x not cleanly
  ## correspond to rotation along the "length" dimension of the tunnel, the
  ## above can be made more specific via:
  ## 1) changing arguments of this function to e.g.
  ## tunnel_length = "position_z"
  ## 2) changing sub() behavior to: sub(tunnel_length,"position_length",namez)

  ## Assign names
  namez -> names(obj_name)

  ## Leave a note that the axes have been renamed
  attr(obj_name,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "renamed_tunnel")

  ## Export
  return(obj_name)
}


############################# gather_tunnel_data ###############################

#' Gather data columns into key-value pairs
#'
#' Reformat \code{viewr} data into a "tidy" format so that every row corresponds
#' to the position (and potentially rotation) of a single subject during an
#' observed frame and time.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param NA_drop Should rows with NAs be dropped? Defaults to \code{TRUE}
#' @param ... Additional arguments that can be passed to other \code{pathviewR}
#' functions such as \code{relabel_viewr_axes()} or \code{read_motive_csv()}
#'
#' @details The tibble or data.frame that is fed in must have variables that
#' have subject names and axis names separated by underscores. Axis names must
#' be one of the following: \code{position_length}, \code{position_width}, or
#' \code{position_height}. Each of these three dimensions must be present in the
#' data. Collectively, this means that names like \code{bird01_position_length}
#' or \code{larry_position_height} are acceptable, but \code{bird01_x} or
#' \code{bird01_length} are not.
#'
#' @return A tibble in "tidy" format which is formatted to have every row
#' correspond to the position (and potentially rotation) of a single subject
#' during an observed frame and time. Subjects' names are automatically parsed
#' from original variable names (e.g. subject1_rotation_width extracts
#' "subject1" as the subject name) and stored in a \code{Subjects} column in the
#' returned tibble.
#'
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the Motive example data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' ## First use relabel_viewr_axes() to rename these variables using _length,
#' ## _width, and _height instead
#' motive_data_relabeled <- relabel_viewr_axes(motive_data)
#'
#' ## Now use gather_tunnel_data() to gather colums into tidy format
#' motive_data_gathered <- gather_tunnel_data(motive_data_relabeled)
#'
#' ## Column names reflect the way in which data were reformatted:
#' names(motive_data_gathered)

gather_tunnel_data <- function(obj_name,
                               NA_drop = TRUE,
                               ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## NOTE: I know that the following 3 blocks of code can be written more
  ## efficiently, but I would rather split them up explicitly so that a user
  ## knows exactly which type of column is missing, given the error message
  ## that is returned.

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Get number of unique subjects
  bodiez <- length(attributes(obj_name)$subject_names_simple)
  if (bodiez == 0) {
    stop("No rigid bodies or markers detected. Please assess your data.")
  }

  ## And the unique subjects' names
  subbiez <- attributes(obj_name)$subject_names_simple

  ## Start setting up a gathered data.frame
  ## Just frame, time, and subject for now
  ## Other columns will be appended
  gathered_data <- data.frame(
    frame = c(rep(obj_name$"frame", bodiez)),
    time_sec = c(rep(obj_name$"time_sec", bodiez))
    )
  ## The data.frame will be dim(obj_name)[1] * bodiez in length
  rb <- NULL
  for (i in 1:bodiez){
    rb <- c(rb,
            rep(subbiez[i], dim(obj_name)[1])
            )
  }
  gathered_data$subject <- rb

  ## Gather positions
    ## Lengths
    tmp_len <-
      obj_name[,grepl("position_length", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$position_length <- tmp_len$value
    ## Widths
    tmp_wid <-
      obj_name[,grepl("position_width", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$position_width <- tmp_wid$value
    ## Heights
    tmp_hei <-
      obj_name[,grepl("position_height", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$position_height <- tmp_hei$value

  ## Gather rotations
    ## Lengths
    tmp_rotl <-
      obj_name[,grepl("rotation_length", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_length <- tmp_rotl$value
    ## Widths
    tmp_rotw <-
      obj_name[,grepl("rotation_width", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_width <- tmp_rotw$value
    ## Heights
    tmp_roth <-
      obj_name[,grepl("rotation_height", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_height <- tmp_roth$value
    ## W
    tmp_rotw <-
      obj_name[,grepl("rotation_real", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$rotation_real <- tmp_rotw$value

  ## Gather mean marker error
    tmp_mark <-
      obj_name[,grepl("mean_marker_error", colnames(obj_name),
                      ignore.case = FALSE)] %>%
      tidyr::gather()
    gathered_data$mean_marker_error <- tmp_mark$value

  ## Drop NAs if desired
    if (NA_drop == TRUE) {
      gathered_data <- gathered_data %>% tidyr::drop_na()
    } else {
      gathered_data <- gathered_data
    }

  ## Coerce to tibble
  gathered_data <- tibble::as_tibble(gathered_data)

  ## Add metadata as attributes()
  attributes_list <-
    c("file_id", "file_mtime", "frame_rate", "header",
      "Motive_IDs", "subject_names_full", "subject_names_simple",
      "data_names", "data_types_full", "data_types_simple" ,
      "d1", "d2", "import_method")

  for (i in seq_len(length(attributes_list))){
    attr(gathered_data, attributes_list[i]) <-
      attr(obj_name, attributes_list[i])
  }

  ## Leave a note that we reshaped the data
  attr(gathered_data,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "gathered_tunnel")

  ## Export
  return(gathered_data)
}


############################# rescale_tunnel_data #############################

#' Rescale position data within a \code{viewr} object
#'
#' Should data have been exported at an incorrect scale, apply an isometric
#' transformation to the position data and associated mean marker errors (if
#' found)
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"} that has been passed
#'   through \code{relabel_viewr_axes()} and \code{gather_tunnel_data()} (or is
#'   structured as though it has been passed through those functions).
#' @param original_scale The original scale at which data were exported. See
#'   Details if unknown.
#' @param desired_scale The desired scale
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @details The \code{desired_scale} is divided by the \code{original_scale} to
#'   determine a \code{scale_ratio} internally. If the \code{original_scale} is
#'   not explicitly known, set it to 1 and then set \code{desired_scale} to be
#'   whatever scaling ratio you have in mind. E.g. setting \code{original_scale}
#'   to 1 and then \code{desired_scale} to 0.7 will multiply all position axis
#'   values by 0.7.
#'
#'   The default arguments of \code{original_scale = 0.5} and
#'   \code{desired_scale = 1} apply a doubling of tunnel size isometrically.
#'
#' @return A \code{viewr} object that has position data (and
#'   \code{mean_marker_error data}, if found) adjusted by the ratio of
#'   \code{desired_scale/original_scale}.
#'
#' @author Vikram B. Baliga
#'
#' @export
#'
#' @examples
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' ## Clean the file. It is generally recommended to clean up to the
#' ## "gather" step before running rescale_tunnel_data().
#'  motive_gathered <-
#'    motive_data %>%
#'    relabel_viewr_axes() %>%
#'    gather_tunnel_data()
#'
#' ## Now rescale the tunnel data
#' motive_rescaled <-
#'   motive_gathered %>%
#'   rescale_tunnel_data(original_scale = 0.5,
#'                       desired_scale = 1)
#'
#' ## See the difference in data range e.g. for length
#' range(motive_rescaled$position_length)
#' range(motive_gathered$position_length)


rescale_tunnel_data <- function(obj_name,
                                original_scale = 0.5,
                                desired_scale = 1,
                                ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }
  ## Scales should be numeric
  if (!is.numeric(original_scale)) {
    stop("original_scale should be numeric")
  }
  if (!is.numeric(desired_scale)) {
    stop("desired_scale should be a numeric")
  }

  ## Compute scale ratio
  scale_ratio <- desired_scale / original_scale

  ## Make a backup
  obj_new <- obj_name

  ## Adjust positions by scale ratio
  obj_new <-
    obj_name %>%
    dplyr::mutate(
      position_length = position_length * scale_ratio,
      position_width  = position_width  * scale_ratio,
      position_height = position_height * scale_ratio
    )

  ## If mean marker error exists, adjust it too
  if ("mean_marker_error" %in% names(obj_name)) {
    obj_new <-
      obj_new %>%
      dplyr::mutate(
        mean_marker_error = mean_marker_error * scale_ratio
        )
  }

  ## Leave a note that we rescaled the data
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "data_rescaled")

  return(obj_new)
}

########################### rename_viewr_characters ############################

#' Rename subjects in the data via pattern detection
#'
#' Quick utility function to use str_replace with mutate(across()) to batch-
#' rename subjects via pattern detection.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param target_column The target column; defaults to "subject"
#' @param pattern The (regex) pattern to be replaced
#' @param replacement The replacement text. Must be a character
#'
#' @return A tibble or data frame in which subjects have been renamed according
#'   to the \code{pattern} and \code{replacement} supplied by the user.
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#'
#' @export
#'
#' @examples
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' ## Clean the file. It is generally recommended to clean up to the
#' ## "gather" step before running rescale_tunnel_data().
#'  motive_gathered <-
#'    motive_data %>%
#'    relabel_viewr_axes() %>%
#'    gather_tunnel_data()
#'
#' ## See the subject names
#'  unique(motive_gathered$subject)
#'
#' ## Now rename the subjects. We'll get rid of "device" and replace it
#' ## with "subject"
#' motive_renamed <-
#'   motive_gathered %>%
#'   rename_viewr_characters(target_column = "subject",
#'                           pattern = "device",
#'                           replacement = "subject")
#'
#' ## See the new subject names
#' unique(motive_renamed$subject)

rename_viewr_characters <- function(obj_name,
                                    target_column = "subject",
                                    pattern,
                                    replacement = ""){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

#   ## Check that gather_tunnel_data() has been run on the object
#   if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
#     stop("You must gather your party before venturing forth.
# Please use gather_tunnel_data() on this object to gather data columns
# into key-value pairs ")
#   }

  ## Check that target_column exists
  if (!target_column %in% names(obj_name)) {
    stop("target_column was not found")
  }

  obj_new <-
    obj_name %>%
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(target_column),
                    stringr::str_replace,
                    pattern,
                    replacement)
    )

  if (target_column == "subject"){
    snf <- attr(obj_name, "subject_names_full")
    sns <- attr(obj_name, "subject_names_simple")

    snf_renamed <- stringr::str_replace(snf, pattern, replacement)
    sns_renamed <- stringr::str_replace(sns, pattern, replacement)

    attr(obj_new, "subject_names_full") <- snf_renamed
    attr(obj_new, "subject_names_simple") <- sns_renamed
  }

  ## Leave a note that we renamed something
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "renamed_characters")

  return(obj_new)

}

############################ trim_tunnel_outliers ##############################

#' Trim out artifacts and other outliers from the extremes of the tunnel
#'
#' The user provides estimates of min and max values of data. This function then
#' trims out anything beyond these estimates.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"} that has been passed
#'   through \code{relabel_viewr_axes()} and \code{gather_tunnel_data()} (or is
#'   structured as though it has been passed through those functions).
#' @param lengths_min Minimum length
#' @param lengths_max Maximum length
#' @param widths_min Minimum width
#' @param widths_max Maximum width
#' @param heights_min Minimum height
#' @param heights_max Maximum height
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @details Anything supplied to _min or _max arguments should be single numeric
#'   values.
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) in which data outside
#'   the specified ranges has been excluded.
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#'
#' @export
#'
#' @examples
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                               package = 'pathviewR'))
#'
#' ## Clean the file. It is generally recommended to clean up to the
#' ## "gather" step before running trim_tunnel_outliers().
#' motive_gathered <-
#'   motive_data %>%
#'   relabel_viewr_axes() %>%
#'   gather_tunnel_data()
#'
#' ## Now trim outliers using default values
#' motive_trimmed <-
#'   motive_gathered %>%
#'   trim_tunnel_outliers(lengths_min = 0,
#'                        lengths_max = 3,
#'                        widths_min = -0.4,
#'                        widths_max = 0.8,
#'                        heights_min = -0.2,
#'                        heights_max = 0.5)

trim_tunnel_outliers <- function(obj_name,
                                 lengths_min = 0,
                                 lengths_max = 3,
                                 widths_min = -0.4,
                                 widths_max = 0.8,
                                 heights_min = -0.2,
                                 heights_max = 0.5,
                                 ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

#   ## Check that gather_tunnel_data() has been run on the object
#   if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
#     stop("You must gather your party before venturing forth.
# Please use gather_tunnel_data() on this object to gather data columns
# into key-value pairs ")
#   }

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Filter by heights first, since extremes in this axis tend to be the
  ## noisiest
  filt_heights <-
    obj_name %>%
    dplyr::filter(position_height < heights_max) %>%
    dplyr::filter(position_height > heights_min)

  ## Now filter by lengths
  filt_lengths <-
    filt_heights %>%
    dplyr::filter(position_length < lengths_max) %>%
    dplyr::filter(position_length > lengths_min)

  ## Finally filter by widths; this may not change anything since outliers
  ## in this axis seem to be rare
  filt_widths <-
    filt_lengths %>%
    dplyr::filter(position_width < widths_max) %>%
    dplyr::filter(position_width > widths_min) %>%
    tibble::as_tibble()

  ## Add metadata as attributes()
  attr(obj_name,"file_id") ->       attr(filt_widths,"file_id")
  attr(obj_name,"file_mtime") ->    attr(filt_widths,"file_mtime")
  attr(obj_name,"frame_rate") ->    attr(filt_widths,"frame_rate")
  attr(obj_name,"header") ->        attr(filt_widths,"header")
  attr(obj_name,"rigid_bodies") ->  attr(filt_widths,"rigid_bodies")
  attr(obj_name,"data_names") ->    attr(filt_widths,"data_names")
  attr(obj_name,"d1") ->            attr(filt_widths,"d1")
  attr(obj_name,"d2") ->            attr(filt_widths,"d2")
  attr(obj_name,"import_method") -> attr(filt_widths,"import_method")

  ## Leave a note that we trimmed tunnel outliers
  attr(filt_widths,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "artifacts_removed")

  ## Export
  return(filt_widths)
}


################################ rotate_tunnel #################################

#' Rotate a tunnel so that perches are approximately aligned
#'
#' The rotation is applied about the height axis and affects tunnel length and
#' width only, i.e. no rotation of height.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"} that has been passed
#'   through \code{relabel_viewr_axes()} and \code{gather_tunnel_data()} (or is
#'   structured as though it has been passed through those functions).
#' @param all_heights_min Minimum perch height
#' @param all_heights_max Maximum perch height
#' @param perch1_len_min Minimum length value of perch 1
#' @param perch1_len_max Maximum length value of perch 1
#' @param perch2_len_min Minimum length value of perch 2
#' @param perch2_len_max Maximum length value of perch 2
#' @param perch1_wid_min Minimum width value of perch 1
#' @param perch1_wid_max Maximum width value of perch 1
#' @param perch2_wid_min Minimum width value of perch 2
#' @param perch2_wid_max Maximum width value of perch 2
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @details The user first estimates the locations of the perches by specifying
#'   bounds for where each perch is located. The function then computes the
#'   center of each bounding box and estimates that to be the midpoint of each
#'   perch. Then the center point of the tunnel (center between the perch
#'   midpoints) is estimated. The angle between perch1_center,
#'   tunnel_center_point, and arbitrary point along the length axis
#'   (tunnel_center_point - 1 on length) is estimated. That angle is then used
#'   to rotate the data, again only in the length and width dimensions. Height
#'   is standardized by (approximate) perch height; values greater than 0 are
#'   above the perch and values less than 0 are below the perch level.
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) in which data have
#'   been rotated according to user specifications.
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#' @family tunnel standardization functions
#'
#' @export
#'
#' @examples
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                               package = 'pathviewR'))
#'
#' ## Clean the file. It is generally recommended to clean up to the
#' ## "trimmed" step before running rotate_tunnel().
#' motive_trimmed <-
#'   motive_data %>%
#'   relabel_viewr_axes() %>%
#'   gather_tunnel_data() %>%
#'   trim_tunnel_outliers()
#'
#' ## Now rotate the tunnel using default values
#' motive_rotated <-
#'   motive_trimmed %>%
#'   rotate_tunnel()
#'
#' ## The following attributes store information about
#' ## how rotation & translation was applied
#' attr(motive_rotated, "rotation_degrees")
#' attr(motive_rotated, "rotation_radians")
#' attr(motive_rotated, "perch1_midpoint_original")
#' attr(motive_rotated, "perch1_midpoint_current")
#' attr(motive_rotated, "tunnel_centerpoint_original")
#' attr(motive_rotated, "perch2_midpoint_original")
#' attr(motive_rotated, "perch2_midpoint_current")

rotate_tunnel <- function(obj_name,
                          all_heights_min = 0.11,
                          all_heights_max = 0.3,
                          ## perch 1 = left (near length = 0); perch 2 = right
                          perch1_len_min = -0.06,
                          perch1_len_max = 0.06,
                          perch2_len_min = 2.48,
                          perch2_len_max = 2.6,
                          perch1_wid_min = 0.09,
                          perch1_wid_max = 0.31,
                          perch2_wid_min = 0.13,
                          perch2_wid_max = 0.35,
                          ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

#   ## Check that gather_tunnel_data() has been run on the object
#   if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
#     stop("You must gather your party before venturing forth.
# Please use gather_tunnel_data() on this object to gather data columns
# into key-value pairs ")
#   }

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }


  ## Now compute the approximate midpoints of each perch
  ## Perch 1 first
  perch1_len <- mean(range(perch1_len_min, perch1_len_max))
  perch1_wid <- mean(range(perch1_wid_min, perch1_wid_max))
  perch1_hei <- mean(range(all_heights_min, all_heights_max))
  perch1_midpoint <- c(perch1_len, perch1_wid, perch1_hei)
  ## Now Perch 2
  perch2_len <- mean(range(perch2_len_min, perch2_len_max))
  perch2_wid <- mean(range(perch2_wid_min, perch2_wid_max))
  perch2_hei <- mean(range(all_heights_min, all_heights_max))
  perch2_midpoint <- c(perch2_len, perch2_wid, perch2_hei)

  ## Now approximate the centerpoint of the tunnel
  tunnel_centerpoint <- c(mean(c(perch1_len, perch2_len)), #length
                          mean(c(perch1_wid, perch2_wid)), #width
                          mean(c(perch1_hei, perch2_hei))  #height
                          )

  ## Define an arbitrary point that is in line with the tunnel center on the
  ## width and height axes but an arbitrary distance (say 1 m) away. This will
  ## be necessary for the angular calculation.
  tunnel_arbitrary <- c(tunnel_centerpoint[1] - 1,
                        tunnel_centerpoint[2],
                        tunnel_centerpoint[3])

  ## Now calculate the angle of which the tunnel is offset
  ## Point 1 = perch1_midpoint
  ## Point 2 = tunnel_centerpoint
  ## Point 3 = tunnel_arbitrary
  ## Using get_2d_angle() instead its 3D counterpart because we're gonna keep
  ## height constant throughout
  tunnel_angle <- get_2d_angle(x1 = perch1_len,
                           y1 = perch1_wid,
                           x2 = tunnel_centerpoint[1],
                           y2 = tunnel_centerpoint[2],
                           x3 = tunnel_arbitrary[1],
                           y3 = tunnel_arbitrary[2])

  ## Now convert to radians, which will be used during the rotation later
  alpharad <- deg_2_rad(tunnel_angle)

  ## Now translate all data in Length and Width so that the tunnel centerpoint
  ## is at (0,0) for (length, width). Keep heights as-is for now?

  ## First the principal points:
  perch1_midpoint_translated <- perch1_midpoint - tunnel_centerpoint
  perch2_midpoint_translated <- perch2_midpoint - tunnel_centerpoint
  tunnel_centerpoint_translated <- tunnel_centerpoint - tunnel_centerpoint
  tunnel_arbitrary_translated <- tunnel_arbitrary - tunnel_centerpoint

  ## Compute the new locations of perch midpoint estimates
  perch1_trans_prime <- c((perch1_midpoint_translated[1]*cos(-1*alpharad))-
                           (perch1_midpoint_translated[2])*sin(-1*alpharad),
                         (perch1_midpoint_translated[1]*sin(-1*alpharad))+
                           (perch1_midpoint_translated[2])*cos(-1*alpharad),
                         perch1_midpoint[3])

  perch2_trans_prime <- c((perch2_midpoint_translated[1]*cos(-1*alpharad))-
                           (perch2_midpoint_translated[2])*sin(-1*alpharad),
                         (perch2_midpoint_translated[1]*sin(-1*alpharad))+
                           (perch2_midpoint_translated[2])*cos(-1*alpharad),
                         perch2_midpoint[3])

  ## Now rotate data in length and width dimensions only
  ## First make a copy of the data. The original data (obj_name) will be used
  ## for the translation step. The new copy (obj_new) will then take that
  ## translated data and rotate it. This is because both length and width
  ## data are simultaneously used during the rotation, so overwriting lengths
  ## will necessarily mess things up for re-defining widths
  obj_name -> obj_new

  ## Now apply a translation to the original data set
  ## (Translation for height in next block of code)
  obj_name$position_length <- obj_name$position_length - tunnel_centerpoint[1]
  obj_name$position_width <- obj_name$position_width - tunnel_centerpoint[2]

  # Now apply a rotation to the translated data set
  obj_new$position_length <- (obj_name$position_length*cos(-1*alpharad))-
    (obj_name$position_width)*sin(-1*alpharad)
  obj_new$position_width <- (obj_name$position_length*sin(-1*alpharad))+
    (obj_name$position_width)*cos(-1*alpharad)
  ## Height will simply be translated
  obj_new$position_height <- obj_name$position_height - tunnel_centerpoint[3]
  ## (all other variables should remain the same)

  ## Coerce to tibble
  obj_new <- tibble::as_tibble(obj_new)

  ## Add new info to attributes that lists the original (approximate) perch
  ## positions, tunnel center point, angle of rotation, and new (approxmate)
  ## perch positions after rotation
  attr(obj_new,"perch1_midpoint_original") <- perch1_midpoint
  attr(obj_new,"perch2_midpoint_original") <- perch2_midpoint
  attr(obj_new,"tunnel_centerpoint_original") <- tunnel_centerpoint
  attr(obj_new,"rotation_degrees") <- tunnel_angle
  attr(obj_new,"rotation_radians") <- alpharad
  attr(obj_new,"perch1_midpoint_current") <- perch1_trans_prime
  attr(obj_new,"perch2_midpoint_current") <- perch2_trans_prime

  ## Leave a note that we rotated and translated the data set
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), c("tunnel_rotated", # rotated
                                          "tunnel_centered") # centered
    )

  ## Export
  return(obj_new)
}


############################# standardize_tunnel ###############################

#' Rotate and center a tunnel based on landmarks
#'
#' Similar to \code{rotate_tunnel()}; rotate and center tunnel data based on
#' landmarks (specific subjects in the data).
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"} that has been passed
#'   through \code{relabel_viewr_axes()} and \code{gather_tunnel_data()} (or is
#'   structured as though it has been passed through those functions).
#' @param landmark_one Subject name of the first landmark
#' @param landmark_two Subject name of the second landmark
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @details The center point of the tunnel is estimated as the point between the
#'   two landmarks. It is therefore recommended that \code{landmark_one} and
#'   \code{landmark_two} be objects that are placed on opposite ends of the
#'   tunnel (e.g. in an avian flight tunnel, these landmarks may be perches that
#'   are placed at the extreme ends). The angle between landmark_one,
#'   tunnel_center_point, and arbitrary point along the length axis
#'   (tunnel_center_point - 1 on length) is estimated. That angle is then used
#'   to rotate the data, again only in the length and width dimensions. Height
#'   is standardized by average landmark height; values greater than 0 are above
#'   the landmarks and values less than 0 are below the landmark level.
#'
#' @section Warning:
#' The \code{position_length} values of landmark_one MUST be less than
#' the \code{position_length} values of landmark_two; otherwise,
#' the rotation will apply to a mirror-image of the tunnel
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) in which data have
#'   been rotated according to the positions of the landmarks in the data.
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#' @family tunnel standardization functions
#'
#' @export
#'
#' @examples
#' ## Example data that would work with this function are
#' ## not included in this version of pathviewR. Please
#' ## contact the package authors for further guidance
#' ## should you need it.

standardize_tunnel <- function(obj_name,
                               landmark_one = "perch1",
                               landmark_two = "perch2",
                               ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

#   ## Check that gather_tunnel_data() has been run on the object
#   if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
#     stop("You must gather your party before venturing forth.
# Please use gather_tunnel_data() on this object to gather data columns
# into key-value pairs ")
#   }

  ## Check that "position_length" exists in at least one column
  if (!any(grepl("position_length",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_length column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_width" exists in at least one column
  if (!any(grepl("position_width",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_width column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Check that "position_height" exists in at least one column
  if (!any(grepl("position_height",
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("position_height column(s) are missing.
Please use relabel_viewr_axes() to rename variables as necessary.")
  }

  ## Tunnels are standardized via information from perch positions. We think
  ## that the most reasonable estimate for perches' positions are their median
  ## values. So the next few blocks of code will determine the median positions
  ## of each perch and then estimate the centerpoint of the tunnel to be between
  ## the two perches (i.e. mean of each set of perch positions).

  ## Make a data.frame of landmark_one (i.e. perch #1)'s positions
  landmark1_med_pos <- obj_name %>%
    dplyr::filter(subject == landmark_one) %>%
    dplyr::summarise(med_length = median(position_length),
                     med_width = median(position_width),
                     med_height = median(position_height)) %>%
    as.data.frame()

  landmark2_med_pos <- obj_name %>%
    dplyr::filter(subject == landmark_two) %>%
    dplyr::summarise(med_length = median(position_length),
                     med_width = median(position_width),
                     med_height = median(position_height)) %>%
    as.data.frame()

  ## Check that perch1_position_length < perch2_position_length; otherwise,
  ## the rotation will apply to a mirror-image of the tunnel
  if (landmark1_med_pos$med_length > landmark2_med_pos$med_length) {
    ## switch the identities of the landmarks
    rename2 <- landmark_one
    landmark_one <- landmark_two
    landmark_two <- rename2

    ## recompute above metrics
    landmark1_med_pos <- obj_name %>%
      dplyr::filter(subject == landmark_one) %>%
      dplyr::summarise(
        med_length = median(position_length),
        med_width = median(position_width),
        med_height = median(position_height)
      ) %>%
      as.data.frame()

    landmark2_med_pos <- obj_name %>%
      dplyr::filter(subject == landmark_two) %>%
      dplyr::summarise(
        med_length = median(position_length),
        med_width = median(position_width),
        med_height = median(position_height)
      ) %>%
      as.data.frame()
  }

  ## Now approximate the centerpoint of the tunnel
  tunnel_centerpoint <-
    rbind(landmark1_med_pos, landmark2_med_pos) %>% colMeans()

  ## Define an arbitrary point that is in line with the tunnel center on the
  ## width and height axes but an arbitrary distance (say 1 m) away. This will
  ## later be necessary for the angular calculation.
  tunnel_arbitrary <- c(tunnel_centerpoint[1] - 1,
                        tunnel_centerpoint[2],
                        tunnel_centerpoint[3])

  ## Now calculate the angle of which the tunnel is offset
  ## Point 1 = perch1_midpoint
  ## Point 2 = tunnel_centerpoint
  ## Point 3 = tunnel_arbitrary
  ## Using get_2d_angle() instead its 3D counterpart because we're gonna keep
  ## height constant throughout
  tunnel_angle <- get_2d_angle(x1 = as.numeric(landmark1_med_pos)[1],
                           y1 = as.numeric(landmark1_med_pos)[2],
                           x2 = tunnel_centerpoint[1],
                           y2 = tunnel_centerpoint[2],
                           x3 = tunnel_arbitrary[1],
                           y3 = tunnel_arbitrary[2])

  ## Now convert to radians, which will be used during the rotation later
  alpharad <- deg_2_rad(tunnel_angle)

  ## Now translate all data in Length and Width so that the tunnel centerpoint
  ## is at (0,0) for (length, width). Keep heights as-is for now?

  ## First the principal points:
  perch1_midpoint_translated <- as.numeric(
    as.numeric(landmark1_med_pos) - tunnel_centerpoint)
  perch2_midpoint_translated <- as.numeric(
    as.numeric(landmark2_med_pos) - tunnel_centerpoint)
  tunnel_centerpoint_translated <- tunnel_centerpoint - tunnel_centerpoint
  ## tunnel_centerpoint_translated should be (0, 0, 0)
  tunnel_arbitrary_translated <- tunnel_arbitrary - tunnel_centerpoint
  ## tunnel_abitrary_translated should be (-1, 0, 0)

  ## Compute the new locations of perch midpoint estimates
  perch1_trans_prime <- c((perch1_midpoint_translated[1]*cos(-1*alpharad))-
                            (perch1_midpoint_translated[2])*sin(-1*alpharad),
                          (perch1_midpoint_translated[1]*sin(-1*alpharad))+
                            (perch1_midpoint_translated[2])*cos(-1*alpharad),
                          as.numeric(landmark1_med_pos)[3])

  perch2_trans_prime <- c((perch2_midpoint_translated[1]*cos(-1*alpharad))-
                            (perch2_midpoint_translated[2])*sin(-1*alpharad),
                          (perch2_midpoint_translated[1]*sin(-1*alpharad))+
                            (perch2_midpoint_translated[2])*cos(-1*alpharad),
                          as.numeric(landmark2_med_pos)[3])

  ## Now rotate data in length and width dimensions only
  ## First make a copy of the data. The original data (obj_name) will be used
  ## for the translation step. The new copy (obj_new) will then take that
  ## translated data and rotate it. This is because both length and width
  ## data are simultaneously used during the rotation, so overwriting lengths
  ## will necessarily mess things up for re-defining widths
  obj_name -> obj_new

  ## Now apply a translation to the original data set
  ## (Translation for height in next block of code)
  obj_name$position_length <- obj_name$position_length - tunnel_centerpoint[1]
  obj_name$position_width <- obj_name$position_width - tunnel_centerpoint[2]

  # Now apply a rotation to the translated data set
  obj_new$position_length <- (obj_name$position_length*cos(-1*alpharad))-
    (obj_name$position_width)*sin(-1*alpharad)
  obj_new$position_width <- (obj_name$position_length*sin(-1*alpharad))+
    (obj_name$position_width)*cos(-1*alpharad)
  ## Height will simply be translated
  obj_new$position_height <- obj_name$position_height - tunnel_centerpoint[3]
  ## (all other variables should remain the same)

  ## Coerce to tibble
  obj_new <- tibble::as_tibble(obj_new)

  ## Add new info to attributes that lists the original (approximate) perch
  ## positions, tunnel center point, angle of rotation, and new (approxmate)
  ## perch positions after rotation
  attr(obj_new,"landmark1_midpoint_original") <- as.numeric(landmark1_med_pos)
  attr(obj_new,"landmark2_midpoint_original") <- as.numeric(landmark2_med_pos)
  attr(obj_new,"tunnel_centerpoint_original") <- tunnel_centerpoint
  attr(obj_new,"rotation_degrees") <- tunnel_angle
  attr(obj_new,"rotation_radians") <- alpharad
  attr(obj_new,"landmark1_midpoint_current") <- perch1_trans_prime
  attr(obj_new,"landmark2_midpoint_current") <- perch2_trans_prime

  ## Leave a note that we rotated and translated the data set
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), c("tunnel_rotated", # rotated
                                          "tunnel_centered") # centered
    )

  ## Export
  return(obj_new)
}

############################# redefine_tunnel_center ###########################

#' "Center" the tunnel data, i.e. translation but no rotation
#'
#' Redefine the center \code{(0, 0, 0,)} of the tunnel data via translating
#' positions along axes.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param axes Names of axes to be centered
#' @param length_method Method for length
#' @param width_method Method for width
#' @param height_method Method for height
#' @param length_zero User-defined value
#' @param width_zero User-defined value
#' @param height_zero User-defined value
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @details
#' For each \code{_method} argument, there are four choices of how centering is
#' handled: 1) "original" keeps axis as is -- this is how width and (possibly)
#' height should be handled for flydra data; 2) "middle" is the middle of the
#' range of data: (min + max) / 2; 3) "median" is the median value of data on
#' that axis. Probably not recommended; and 4) "user-defined" lets the user
#' customize where the (0, 0, 0) point in the tunnel will end up. Each
#' \code{_zero} argument is subtracted from its corresponding axis' data.
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) in which data have
#'   been translated according to the user's inputs, generally with \code{(0, 0,
#'   0,)} being relocated to the center of the tunnel.
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#' @family tunnel standardization functions
#'
#' @export
#'
#' @examples
#' ## Import the Flydra example data included in
#' ## the package
#' flydra_data <-
#'   read_flydra_mat(
#'     system.file("extdata",
#'                 "pathviewR_flydra_example_data.mat",
#'                 package = 'pathviewR'),
#'     subject_name = "birdie_wooster"
#'   )
#'
#' ## Re-center the Flydra data set.
#' ## Width will be untouched
#' ## Length will use the "middle" definition
#' ## And height will be user-defined to be
#' ## zeroed at 1.44 on the original axis
#' flydra_centered <-
#'   flydra_data %>%
#'   redefine_tunnel_center(length_method = "middle",
#'                          height_method = "user-defined",
#'                          height_zero = 1.44)


redefine_tunnel_center <-
  function(obj_name,
           axes = c("position_length", "position_width", "position_height"),
           length_method = c("original", "middle", "median", "user-defined"),
           width_method  = c("original", "middle", "median", "user-defined"),
           height_method = c("original", "middle", "median", "user-defined"),
           length_zero = NA,
           width_zero = NA,
           height_zero = NA,
           ...) {

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

#   ## Check that gather_tunnel_data() has been run on the object
#   if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
#     stop("You must gather your party before venturing forth.
# Please use gather_tunnel_data() on this object to gather data columns
# into key-value pairs ")
#   }

  ## Check that each column exists
  if (!"position_length" %in% names(obj_name)) {
    stop("Length column not found.
This column must be named 'position_length'.")
  }
  if (!"position_width" %in% names(obj_name)) {
    stop("Width column not found
This column must be named 'position_width'.")
  }
  if (!"position_height" %in% names(obj_name)) {
    stop("Height column not found
This column must be named 'position_height'.")
  }

  ## NEED TO ADD: Check for NAs within the data columns:
  # if (SOMETHING) == TRUE){
  #   stop("NA values found within data, please clean")
  # }

  length_method <- match.arg(length_method)
  width_method  <- match.arg(width_method)
  height_method <- match.arg(height_method)

  ## Summarize each dimension
  length_min <- min(obj_name$position_length)
  length_max <- max(obj_name$position_length)
  length_middle <- (length_max + length_min)/2
  length_median <- median(obj_name$position_length)
  width_min <- min(obj_name$position_width)
  width_max <- max(obj_name$position_width)
  width_middle <- (width_max + width_min)/2
  width_median <- median(obj_name$position_width)
  height_min <- min(obj_name$position_height)
  height_max <- max(obj_name$position_height)
  height_middle <- (height_max + height_min)/2
  height_median <- median(obj_name$position_height)

  ## Create new object to overwrite
  obj_new <- obj_name

  ## Handle lengths first
  if (length_method == "original"){
    obj_new$position_length <- obj_name$position_length
  }
  if (length_method == "middle"){
    obj_new$position_length <- obj_name$position_length - length_middle
  }
  if (length_method == "median"){
    obj_new$position_length <- obj_name$position_length - length_median
  }
  if (length_method == "user-defined"){
    if (is.character(length_zero) == TRUE){
      stop("length_zero must be a numeric vector of length 1")
    }
    if (is.na(length_zero) == TRUE){
      stop("You must supply a length_zero argument.")
    }
    obj_new$position_length <- obj_name$position_length - length_zero
  }

  ## Handle widths second
  if (width_method == "original"){
    obj_new$position_width <- obj_name$position_width
  }
  if (width_method == "middle"){
    obj_new$position_width <- obj_name$position_width - width_middle
  }
  if (width_method == "median"){
    obj_new$position_width <- obj_name$position_width - width_median
  }
  if (width_method == "user-defined"){
    if (is.character(width_zero) == TRUE){
      stop("width_zero must be a numeric vector of length 1")
    }
    if (is.na(width_zero) == TRUE){
      stop("You must supply a width_zero argument.")
    }
    obj_new$position_width <- obj_name$position_width - width_zero
  }

  ## Handle heights third
  if (height_method == "original"){
    obj_new$position_height <- obj_name$position_height
  }
  if (height_method == "middle"){
    obj_new$position_height <- obj_name$position_height - height_middle
  }
  if (height_method == "median"){
    obj_new$position_height <- obj_name$position_height - height_median
  }
  if (height_method == "user-defined"){
    if (is.character(height_zero) == TRUE){
      stop("height_zero must be a numeric vector of length 1")
    }
    if (is.na(height_zero) == TRUE){
      stop("You must supply a height_zero argument.")
    }
    obj_new$position_height <- obj_name$position_height - height_zero
  }

  ## TO ADD: Note what the original center was in the attributes

  ## Leave a note that we translated the data set
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "tunnel_centered") # centered

## Export
return(obj_new)

}

############################### select_x_percent ###############################

#' Select a region of interest within the tunnel
#'
#' Select data in the middle X percent of the length of the tunnel
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param desired_percent Numeric, the percent of the total length of the tunnel
#'   that will define the region of interest. Measured from the center outwards.
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) in which data outside
#'   the region of interest have been removed.
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#'
#' @export
#'
#' @examples
## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                               package = 'pathviewR'))
#'
#' ## Clean the file. It is generally recommended to clean up to the
#' ## "trimmed" step before running rotate_tunnel().
#' motive_rotated <-
#'   motive_data %>%
#'   relabel_viewr_axes() %>%
#'   gather_tunnel_data() %>%
#'   trim_tunnel_outliers() %>%
#'   rotate_tunnel()
#'
#' ## Now select the middle 50% of the tunnel
#' motive_selected <-
#'   motive_rotated %>%
#'   select_x_percent(desired_percent = 50)
#'
#' ## Compare the ranges of lengths to see the effect
#' range(motive_rotated$position_length)
#' range(motive_selected$position_length)

select_x_percent <- function(obj_name,
                             desired_percent = 33,
                             ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

#   ## Check that it's undergone one of our centering steps
#   if (!any(attr(obj_name,"pathviewR_steps") == "tunnel_centered")) {
#     warning("This viewr object does not seem to have been passed through
# one of our centering options, e.g. rotate_tunnel(), standardize_tunnel(),
# or center_tunnel(). Please proceed with extreme caution.")
#   }

  ## Convert percent to proportion
  prop <- desired_percent/100

  ## Get tunnel length
  tunnel_range <- range(obj_name$position_length)
  tunnel_length <- sum(abs(tunnel_range[1]), abs(tunnel_range[2]))

  ## Determine the range of lengths that will be needed
  ## Multiply tunnel length by the proportion, then divide by 2 to get
  ## the postive (towards the right) and negative (towards the left) halves
  lengths_needed <- (tunnel_length * prop)/2

  ## Now filter by lengths
  obj_name <-
    obj_name %>%
    dplyr::filter(position_length < lengths_needed) %>%
    dplyr::filter(position_length > (-1 * lengths_needed))

  ## Coerce to tibble
  obj_name <- tibble::as_tibble(obj_name)

  ## Check that ROI contains data
  roi <- dim(obj_name)
  if (roi[1] == 0) {
    stop("region of interest does not contain data")
  }

  ## Leave a note about the proportion used
  attr(obj_name,"percent_selected") <- desired_percent
  attr(obj_name,"full_tunnel_length") <- tunnel_length
  attr(obj_name,"selected_tunnel_length") <- tunnel_length * prop

  ## Leave a note that we rotated and translated the data set
  attr(obj_name,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "percent_selected")

  ## Export
  return(obj_name)
}


########################## quick_separate_trajectories #########################

#' Quick version of separate_trajectories()
#'
#' Mostly meant for internal use but available nevertheless.
#'
#' @inheritParams separate_trajectories
#' @param max_frame_gap Unlike the corresponding parameter in
#'   \code{separate_trajectories}, must be a single numeric here.
#'
#' @details This function is designed to separate rows of data into separately
#'   labeled trajectories.
#'
#'   The \code{max_frame_gap} parameter determines how trajectories will be
#'   separated. \code{max_frame_gap} defines the largest permissible gap in data
#'   before a new trajectory is forced to be defined. In this function, only a
#'   single numeric can be supplied to this parameter (unlike the case in
#'   \code{separate_trajectories}).
#'
#' @inherit separate_trajectories return
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#' @family functions that define or clean trajectories
#'
#' @export
#'
#' @examples
#' ## This function is not recommended for general use.
#' ## See separate_trajectories() instead

quick_separate_trajectories <- function(obj_name,
                                        max_frame_gap = 1,
                                        ...){

  sploot <-
    obj_name %>%
    dplyr::select(frame) %>%
    #dplyr::group_by(subject) %>%
    ## group by seq_id which is the diff between successive frames
    ## is greater than max_frame_gap
    dplyr::group_by(seq_id = cumsum(c(1, diff(frame)) > max_frame_gap))

  ## Duplicate obj_name to avoid overwriting important stuff
  obj_new <- obj_name

  ## new column (traj_id) is this seq_id
  obj_new$traj_id <- sploot$seq_id

  ## Also combine the subject ID so that we're sure trajectories
  ## correspond to unique subjects
  obj_new$sub_traj <- paste0(obj_new$subject,"_",obj_new$traj_id)

  ## Coerce to tibble
  obj_new <- tibble::as_tibble(obj_new)

  ## Leave a note about the max frame gap used
  attr(obj_new,"max_frame_gap") <- max_frame_gap

  ## Leave a note that we rotated and translated the data set
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_new,"pathviewR_steps"), "trajectories_labeled")

  ## Export
  return(obj_new)
}


############################# separate_trajectories ############################

#' Separate rows of data into separately labeled trajectories.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param max_frame_gap Default 1; defines the largest permissible gap in data
#'   before a new trajectory is forced to be defined. Can be either a numeric
#'   value or "autodetect". See Details for more.
#' @param frame_rate_proportion Default 0.10; if \code{max_frame_gap =
#'   "autodetect"}, proportion of frame rate to be used as a reference (see
#'   Details).
#' @param frame_gap_messaging Default FALSE; should frame gaps be reported in
#'   the console?
#' @param frame_gap_plotting Default FALSE; should frame gap diagnostic plots be
#'   shown?
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @details This function is designed to separate rows of data into separately
#'   labeled trajectories.
#'
#'   The \code{max_frame_gap} parameter determines how trajectories will be
#'   separated. If numeric, the function uses the supplied value as the largest
#'   permissible gap in frames before a new trajectory is defined.
#'
#'   If \code{max_frame_gap = "autodetect"} is used, the function
#'   attempts to guesstimate the best value(s) of \code{max_frame_gap}. This is
#'   performed separately for each subject in the data set, i.e. as many
#'   \code{max_frame_gap} values will be estimated as there are unique subjects.
#'   The highest possible value of any \code{max_frame_gap} is first set as a
#'   proportion of the capture frame rate, as defined by the
#'   \code{frame_rate_proportion} parameter (default 0.10). For each subject, a
#'   plot of total trajectory counts vs. max frame gap values is created
#'   internally (but can be plotted via setting
#'   \code{frame_gap_plotting = TRUE}). As larger max frame gaps are allowed,
#'   trajectory count will necessarily decrease but may reach a value that
#'   likely represents the "best" option. The "elbow" of that plot is then used
#'   to find an estimate of the best max frame gap value to use.
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) in which a new column
#'   \code{file_sub_traj} is added, which labels trajectories within the data by
#'   concatenating file name, subject name, and a trajectory number (all
#'   separated by underscores).
#'
#' @author Vikram B. Baliga and Melissa S. Armstrong
#'
#' @family data cleaning functions
#' @family functions that define or clean trajectories
#'
#' @importFrom graphics plot
#'
#' @export
#'
#' @examples
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                               package = 'pathviewR'))
#'
#' ## Clean the file. It is generally recommended to clean up to the
#' ## "select" step before running select_x_percent().
#' motive_selected <-
#'   motive_data %>%
#'   relabel_viewr_axes() %>%
#'   gather_tunnel_data() %>%
#'   trim_tunnel_outliers() %>%
#'   rotate_tunnel() %>%
#'   select_x_percent(desired_percent = 50)
#'
#' ## Now separate trajectories using autodetect
#' motive_separated <-
#'   motive_selected %>%
#'   separate_trajectories(max_frame_gap = "autodetect",
#'                         frame_rate_proportion = 0.1)
#'
#' ## See new column file_sub_traj for trajectory labeling
#' names(motive_separated)

separate_trajectories <- function(obj_name,
                                  max_frame_gap = 1,
                                  frame_rate_proportion = 0.10,
                                  frame_gap_messaging = FALSE,
                                  frame_gap_plotting = FALSE,
                                  ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

#   ## Check that gather_tunnel_data() has been run on the object
#   if (!any(attr(obj_name,"pathviewR_steps") == "gathered_tunnel")) {
#     stop("You must gather your party before venturing forth.
# Please use gather_tunnel_data() on this object to gather data columns
# into key-value pairs ")
#   }

  ## Get subject names
  subject_names_simple <- base::unique(obj_name$subject)

  ## Extract the frames and subjects and group by subjects
  grouped_frames <-
    obj_name %>%
    dplyr::select(frame, subject) %>%
    dplyr::group_by(subject)

  ## Split that tibble into a list of tibbles -- one per subject
  splitz <-
    dplyr::group_split(grouped_frames)

  ## Also generate separate subject-specific tibbles of all data
  subject_tibbles <-
    obj_name %>%
    dplyr::group_by(subject) %>%
    dplyr::group_split()

  ## Determine the frame rate of the exported object
  frame_rate <-
    attr(obj_name, "frame_rate") %>% ## 5th line of header
    as.numeric()

  ## Get File ID
  flid <- attr(obj_name, "file_id")

  ## Now determine the maximum gap within each subject's set of frames
  ## This has to be done on a per-subject basis because frame numbering
  ## in obj_name$frame is recycled as new subjects are encountered (going
  ## down the rows)
  maxFGs_by_subject <- NULL
  allFGs_by_subject <- NULL
  for (i in seq_len(length(splitz))){
    maxFGs_by_subject[i] <- max(diff(splitz[[i]]$frame))
    allFGs_by_subject[[i]] <- diff(splitz[[i]]$frame)
  }
  ## Store the largest of these as the maximum frame gap across subjects
  maxFG_across_subjects <- max(maxFGs_by_subject)

  ## If max_frame_gap is a numeric
    if (is.numeric(max_frame_gap)) {
      mufasa <- max_frame_gap
      ## Now check that max_frame_gap does not exceed the
      ## actual max across subjects
      if (mufasa > maxFG_across_subjects) {
        message("Largest frame gap detected exceeds max_frame_gap argument.
Setting max_frame_gap to ", maxFG_across_subjects)
        mufasa <- maxFG_across_subjects
      }

      ## max_frame_gap has now been verified or estimated by this function.
      sploot <-
        obj_name %>%
        dplyr::select(frame, subject) %>%
        ## group by seq_id which is the diff between successive frames
        ## is greater than max_frame_gap
        dplyr::group_by(traj_id = cumsum(c(1, diff(frame)) > mufasa)) %>%
        ## generate a sub_traj
        dplyr::mutate(file_sub_traj = paste0(flid,"_",subject,"_",traj_id))

      ## Duplicate obj_name to avoid overwriting important stuff
      obj_tmp <- obj_name

      ## add new column (sub_traj)
      obj_new <- dplyr::left_join(obj_tmp, sploot,
                                  by = c("frame", "subject"))

      ## Leave a note about the max frame gap used
      attr(obj_new,"max_frame_gap") <- mufasa

      ## Leave a note that we rotated and translated the data set
      attr(obj_new,"pathviewR_steps") <-
        c(attr(obj_new,"pathviewR_steps"), "trajectories_labeled")

      ## Export
      return(obj_new)

    }

  ## If max_frame_gap is set to "autodetect"
  if (max_frame_gap == "autodetect"){
    message("autodetect is an experimental feature -- please report issues.")

    ## Figure out highest number of frame gaps to try
    if (!is.numeric(frame_rate_proportion)){
      stop("frame_rate_proportion must be numeric and between 0 and 1")
    }
    if (frame_rate_proportion > 1 || frame_rate_proportion < 0) {
      stop(
        "frame_rate_proportion must be expressed as a decimal between 0 and 1")
    }
    ## the maximal frame gap cannot exceed a (user-specified) proportion
    ## of the frame_rate. Use floor() to ensure it is an integer.
    max_frame_gap_allowed <-
      base::floor(frame_rate_proportion * frame_rate)

    sploot <- list()
    mufasa <- NULL
    ## For each subject's tibble, run through the process of finding the elbow
    for (i in seq_len(length(subject_tibbles))){
      ## Make a bunch of empty vectors to dump info
      mfg <- NULL ## All must be NULL so they can be re-written with each loop
      cts <- NULL
      trajectory_count <- NULL
      frame_gap_allowed <- NULL

      ## Loop through max frame gap values
      j <- 1
      while (j < max_frame_gap_allowed + 1) {
        mfg[[j]] <- quick_separate_trajectories(subject_tibbles[[i]],
                                               max_frame_gap = j)
        cts[[j]] <- dplyr::count(mfg[[j]], traj_id)
        trajectory_count[j] <- nrow(cts[[j]])
        frame_gap_allowed[j] <- j
        j <- j +1
      }

      ## Collect the info on max frame gaps allowed vs. trajectory counts
      mfg_tib <- tibble::tibble(frame_gap_allowed,
                                trajectory_count)

      ## Find the curve elbow point via `find_curve_elbow()`
      mufasa[[i]] <- find_curve_elbow(mfg_tib,
                                             export_type = "row_num",
                                             plot_curve = FALSE)

      if(frame_gap_messaging == TRUE){
        message("For subject: ", subject_names_simple[i],
", estimated best value for max_frame_gap: ", mufasa[[i]])
      }

      if(frame_gap_plotting == TRUE){
      plot(mfg_tib); graphics::abline(v = mufasa[[i]])
        ## refine this later to specify the subject name & make it pretty etc
      }

      ## max_frame_gap has now been verified or estimated by this function.
      sploot[[i]] <-
        subject_tibbles[[i]] %>%
        dplyr::select(frame, subject) %>%
        ## group by seq_id which is the diff between successive frames
        ## is greater than max_frame_gap
        dplyr::group_by(traj_id = cumsum(c(1, diff(frame)) > mufasa[[i]])) %>%
        ## generate a sub_traj
        dplyr::mutate(file_sub_traj = paste0(flid,"_",subject,"_",traj_id))

    }

      ## Duplicate obj_name to avoid overwriting important stuff
      obj_tmp <- obj_name

      ## Meld sploot together
      new_sploot <- dplyr::bind_rows(sploot)

      ## add new column (sub_traj)
      obj_new <- dplyr::left_join(obj_tmp, new_sploot,
                                  by = c("frame", "subject"))

      ## Leave a note about the max frame gaps used
      attr(obj_new,"max_frame_gap") <- unlist(mufasa)

      ## Leave a note that we rotated and translated the data set
      attr(obj_new,"pathviewR_steps") <-
        c(attr(obj_new,"pathviewR_steps"), "trajectories_labeled")

      ## Export
      return(obj_new)

  }

  }


############################# get_full_trajectories ############################

#' Retain trajectories that span a selected region of interest
#'
#' Specify a minimum span of the selected region of interest and then keep
#' trajectories that are wider than that span and go from one end to the other
#' of the region.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param span Span to use; must be numeric and between 0 and 1
#' @param ... Additional arguments passed to/from other pathviewR functions
#'
#' @details Because trajectories may not have observations exactly at the
#' beginning or the end of the region of interest, it may be necessary to allow
#' trajectories to be slightly shorter than the range of the selected region of
#' interest. The \code{span} parameter of this function handles this. By
#' supplying a numeric proportion from 0 to 1, a user may allow trajectories to
#' span that proportion of the selected region. For example, setting \code{span
#' = 0.95} will keep all trajectories that span 95% of the length of the
#' selected region of interest. Setting \code{span = 1} (not recommended) will
#' strictly keep trajectories that start and end at the exact cut-offs of the
#' selected region of interest. For these reasons, \code{span}s of 0.99 to 0.95
#' are generally recommended.
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}) in which only
#'   trajectories that span the region of interest are retained. Data are
#'   labeled by direction  (either "leftwards" or "rightwards") with respect to
#'   their starting and ending \code{position_length} values in the
#'   \code{direction} column.
#'
#' @author Vikram B. Baliga
#'
#' @family data cleaning functions
#' @family functions that define or clean trajectories
#'
#' @export
#'
#' @examples
## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                               package = 'pathviewR'))
#'
#' ## Clean the file. It is generally recommended to clean up to the
#' ## "separate" step before running select_x_percent().
#' motive_separated <-
#'   motive_data %>%
#'   relabel_viewr_axes() %>%
#'   gather_tunnel_data() %>%
#'   trim_tunnel_outliers() %>%
#'   rotate_tunnel() %>%
#'   select_x_percent(desired_percent = 50) %>%
#'   separate_trajectories(max_frame_gap = "autodetect",
#'                         frame_rate_proportion = 0.1)
#'
#' ## Now retain only the "full" trajectories that span
#' ## across 0.95 of the range of position_length
#' motive_full <-
#'   motive_separated %>%
#'   get_full_trajectories(span = 0.95)

get_full_trajectories <- function(obj_name,
                                  span = 0.8,
                                  ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  # ## Check that its axes have been renamed
  # if (!any(attr(obj_name,"pathviewR_steps") == "trajectories_labeled")) {
  #   stop("Please use separate_trajectories() prior to using this")
  # }

  summary_obj <-
    obj_name %>%
    dplyr::group_by(file_sub_traj) %>%
    dplyr::summarise(traj_length = dplyr::n(),
                     start_length = position_length[1],
                     end_length = position_length[traj_length],
                     length_diff = abs(end_length - start_length),
                     start_length_sign = sign(start_length),
                     end_length_sign = sign(end_length))

  ## Define rightwards as starting at negative length values and going
  ## towards positive
  summary_obj$direction <-
    ifelse(summary_obj$start_length < 0, "rightwards", "leftwards")

  ## If selected lengths have been stripped away, compute the maximum
  ## length again
  if (is.null(attr(obj_name, "selected_tunnel_length"))) {
    max_length <- sum(abs(range(obj_name$position_length)[1]),
                      abs(range(obj_name$position_length)[2]))
  } else {
    # Otherwise, use the selected_tunnel_length
    max_length <- attr(obj_name,"selected_tunnel_length")
  }

  ## Filter data by the two criteria
  filt_summary <-
    summary_obj %>%
    dplyr::group_by(file_sub_traj) %>%
    ## Each trajectory must span a minimum porportion of the selected tunnel
    dplyr::filter(length_diff > (span * max_length)) %>%
    ## And the signs (+ or -) at the ends of the trajectories must be opposites
    dplyr::filter(start_length_sign != end_length_sign)

  obj_continuous <-
    obj_name %>%
    dplyr::filter(file_sub_traj %in% filt_summary$file_sub_traj)

  ## Join the columns to add in direction
  obj_defined <-
    dplyr::right_join(obj_continuous, filt_summary, by = "file_sub_traj") %>%
    tibble::as_tibble()

  ## Leave a note about the span used
  attr(obj_defined, "span") <- span

  ## Leave a note that full trajectories were retained
  attr(obj_defined,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "full_trajectories")

  ## Leave a note about trajectories removed
  attr(obj_defined, "trajectories_removed") <-
    length(summary_obj$file_sub_traj) - length(filt_summary$file_sub_traj)

  ## Export
  return(obj_defined)

}


############################## section_tunnel_by ###############################

#' Bin data along a specified axis
#'
#' Chop data into X sections (of equal size) along a specified axis
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param axis Chosen axis, must match name of column exactly
#' @param number_of_sections Total number of sections
#'
#' @details The idea is to bin the data along a specified axis, generally
#'   \code{position_length}.
#'
#' @return A new column added to the input data object called \code{section_id},
#'   which is an ordered factor that indicates grouping.
#'
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @examples
#' ## Load data and run section_tunnel_by()
#' test_mat <-
#'   read_flydra_mat(system.file("extdata", "pathviewR_flydra_example_data.mat",
#'                              package = 'pathviewR'),
#'                   subject_name = "birdie_wooster") %>%
#'   redefine_tunnel_center(length_method = "middle",
#'                          height_method = "user-defined",
#'                          height_zero = 1.44) %>%
#'   select_x_percent(desired_percent = 50) %>%
#'   separate_trajectories(max_frame_gap = 1) %>%
#'   get_full_trajectories(span = 0.95) %>%
#'   section_tunnel_by(number_of_sections = 10)
#'
#' ## Plot; color by section ID
#' plot(test_mat$position_length,
#'      test_mat$position_width,
#'      asp = 1, col = as.factor(test_mat$section_id))

section_tunnel_by <- function(obj_name,
                              axis = "position_length",
                              number_of_sections = 20){

  ## Find the specified axis
  dimension <- obj_name[, grepl(axis, colnames(obj_name),
                                ignore.case = FALSE)][[1]]

  ## Extract the axis
  vec <- tibble::tibble(x = as.vector(dimension))
  colnames(vec) <- "x"

  ## Now cut using strata defintions and store back in obj_name
  obj_name$section_id <-
    base::cut(x = vec$x,
              breaks = number_of_sections,
              labels = FALSE,
              include.lowest = FALSE,
              right = TRUE,
              ordered_result = TRUE)

  ## Leave a note that this was done
  attr(obj_name, "pathviewR_steps") <-
    c(attr(obj_name, "pathviewR_steps"), "tunnel_sectioned")

  ## Export
  return(obj_name)
}


############################## exclude_by_velocity #############################

#' Remove trajectories entirely, based on velocity thresholds
#'
#' Remove trajectories from a viewr object that contain instances of velocity
#' known to be spurious.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param vel_min Default \code{NULL}. If a numeric is entered, trajectories
#'   that have at least one observation with velocity less than \code{vel_min}
#'   are removed.
#' @param vel_max Default \code{NULL}. If a numeric is entered, trajectories
#'   that have at least one observation with velocity greater than
#'   \code{vel_max} are removed.
#'
#' @return A new viewr object that is identical to the input object but now
#'   excludes any trajectories that contain observations with velocity less than
#'   \code{vel_min} (if specified) and/or velocity greater than \code{vel_max}
#'   (if specified)
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @examples
#' ## Import and clean the example Motive data
#' motive_import_and_clean <-
#'   import_and_clean_viewr(
#'     file_name = system.file("extdata", "pathviewR_motive_example_data.csv",
#'                             package = 'pathviewR'),
#'     desired_percent = 50,
#'     max_frame_gap = "autodetect",
#'     span = 0.95
#'   )
#'
#' ## See the distribution of velocities
#' hist(motive_import_and_clean$velocity)
#'
#' ## Let's remove any trajectories that contain
#' ## velocity < 2
#' motive_vel_filtered <-
#'   motive_import_and_clean %>%
#'   exclude_by_velocity(vel_min = 2)
#'
#' ## See how the distribution of velocities has changed
#' hist(motive_vel_filtered$velocity)


 exclude_by_velocity <- function(obj_name,
                                 vel_min = NULL,
                                 vel_max = NULL){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Get a list of all the unique trajectories
  unique_trajs <- base::unique(obj_name$file_sub_traj)

  ## Remove trajectories with velocities below a threshold
  if (is.numeric(vel_min)) {
    ## Find the data that are below the threshold
    below_thresh <-
      obj_name %>%
      dplyr::filter(velocity < vel_min) %>%
      dplyr::select(file_sub_traj) %>%
      base::unique() %>%
      purrr::as_vector()
    ## Remove these trajectories from the original object
    obj_name <-
      obj_name %>%
      dplyr::filter(!(file_sub_traj %in% below_thresh))
  } else {
    ## if FALSE
    obj_name <- obj_name
    #if is character instead of numeric:
    if (is.character(vel_min)) {
      stop(
        "vel_min is character.
    Please check that you have entered the vel_min as a numeric."
      )
    }
  }

  ## Remove trajectories with velocities above a threshold
  if (is.numeric(vel_max)) {
    ## Find the data that are above the threshold
    above_thresh <-
      obj_name %>%
      dplyr::filter(velocity > vel_max) %>%
      dplyr::select(file_sub_traj) %>%
      base::unique() %>%
      purrr::as_vector()
    ## Remove these trajectories from the original object
    obj_name <-
      obj_name %>%
      dplyr::filter(!(file_sub_traj %in% above_thresh))
  } else {
    ## if FALSE
    obj_name <- obj_name
    #if is character instead of numeric:
    if (is.character(vel_max)) {
      stop(
        "vel_max is character.
    Please check that you have entered the vel_max as a numeric."
      )
    }
  }

  ## Export
  return(obj_name)
}


################################ fill_traj_gaps ################################

#' Interpolate gaps within trajectories
#'
#' Use LOESS smoothing to fill in gaps of missing data within trajectories in
#' a viewr object
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}. Trajectories must be
#'   predefined (i.e. via \code{separate_trajectories()}).
#' @param loess_degree See "degree" argument of fANCOVA::loess.as()
#' @param loess_criterion See "criterion" argument of fANCOVA::loess.as()
#' @param loess_family See "family" argument of fANCOVA::loess.as()
#' @param loess_user_span See "user.span" argument of fANCOVA::loess.as()
#'
#' @details It is strongly recommended that the input viewr object be "cleaned"
#'   via \code{select_x_percent()} -> \code{separate_trajectories()} ->
#'   \code{get_full_trajectories()} prior to using this function. Doing so will
#'   ensure that only trajectories with minor gaps will be used in your
#'   analyses. This function will then enable you to interpolate missing data in
#'   those minor gaps.
#'
#'   Interpolation is handled by first fitting a series of LOESS regressions
#'   (via \code{fANCOVA::loess.as()}). In each regression, a position axis (e.g.
#'   \code{position_length}) is regressed against \code{frame} (\code{frame} is
#'   x-axis). From that relationship, values of missing position data are
#'   determined and then inserted into the original data set.
#'
#'   See \link[fANCOVA]{loess.as} for further details on parameters.
#'
#' @return A viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"} that now includes new
#'   observations (rows) as a result of interpolation to fill in missing data. A
#'   new column \code{gaps_filled} is added to the data to indicate original
#'   data ("No") vs data that have been inserted to fill gaps ("Yes").
#'
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' ## Clean, isolate, and label trajectories
#' motive_full <-
#'   motive_data %>%
#'   clean_viewr(desired_percent = 50,
#'               max_frame_gap = "autodetect",
#'               span = 0.95)
#'
#' ## Interpolate missing data via this function
#' motive_filling <-
#'  motive_full %>%
#'  fill_traj_gaps()
#'
#' ## plot all trajectories (before)
#' plot_viewr_trajectories(motive_full, multi_plot = TRUE)
#' ## plot all trajectories(after)
#' plot_viewr_trajectories(motive_filling, multi_plot = TRUE)

fill_traj_gaps <- function(obj_name,
                           loess_degree = 1,
                           loess_criterion = c("aicc", "gcv"),
                           loess_family = c("gaussian", "symmetric"),
                           loess_user_span = NULL
){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Split the object by file_sub_traj
  obj_splits <-
    obj_name %>%
    base::split(f = obj_name$file_sub_traj)

  ## Re-order the list so the original order is maintained
  obj_splits <-
    obj_splits[unique(obj_name$file_sub_traj)]

  ## Evaluate which trajectories contain gaps
  traj_gap_indicator <- NULL
  for (i in seq_len(length(obj_splits))){
    if(
      ## If a trajectory has gaps
      any(diff(obj_splits[[i]]$frame) > 1)
    ) {
      ## Set traj_gap_indicator entry to TRUE
      traj_gap_indicator[i] <- TRUE
    } else {
      ## If the trajectory has no gaps, set traj_gap_indicator to FALSE
      traj_gap_indicator[i] <- FALSE
    }
  }

  ## Fill gaps
  for (i in seq_len(length(traj_gap_indicator))){

    ## Work only with cases where gaps are found; all other trajectories
    ## should not be touched
    if(traj_gap_indicator[i] == TRUE){

      ## Isolate the trajectory
      gap_dat <- obj_splits[[i]] %>%
        tibble::add_column(gaps_filled = "No")

      ## Generate a sequence from min frame to max frame as though
      ## there were no gaps
      frame_seq <-
        seq(from = min(gap_dat$frame),
            to = max(gap_dat$frame),
            by = 1)

      ## Create a time sequence of the same length
      ## Make it the same length as frame_seq so as to fill in the gap(s)
      time_seq <-
        seq(from = min(gap_dat$time_sec),
            to = max(gap_dat$time_sec),
            length.out = length(frame_seq))

      ## Use fANCOVA::loess.as() to automate the loess fit span

      ## position_length
      length_fit <-
        fANCOVA::loess.as(gap_dat$frame,
                          gap_dat$position_length,
                          degree = loess_degree,
                          criterion = loess_criterion,
                          family = loess_family,
                          user.span = loess_user_span,
                          plot = FALSE)
      length_preds <- predict(length_fit, frame_seq)
      ## position_width
      width_fit <-
        fANCOVA::loess.as(gap_dat$frame,
                          gap_dat$position_width,
                          degree = loess_degree,
                          criterion = loess_criterion,
                          family = loess_family,
                          user.span = loess_user_span,
                          plot = FALSE)
      width_preds <- predict(width_fit, frame_seq)
      ## position_height
      height_fit <-
        fANCOVA::loess.as(gap_dat$frame,
                          gap_dat$position_height,
                          degree = loess_degree,
                          criterion = loess_criterion,
                          family = loess_family,
                          user.span = loess_user_span,
                          plot = FALSE)
      height_preds <- predict(height_fit, frame_seq)

      ## Get all the predicted data together
      predicted_data <-
        tibble::tibble(frame = frame_seq,
                       time_sec = time_seq,
                       position_length = length_preds,
                       position_width = width_preds,
                       position_height = height_preds,
                       gaps_filled = "Yes") %>%
        get_velocity()

      ## join back with the original data
      new_dat <-
        dplyr::left_join(predicted_data, gap_dat, by = c("frame"))

      ## Copy over metadata
      ## We can likely write this better, but doing it this way for now so
      ## I can be sure
for (j in seq_len(nrow(new_dat))) {
  if (is.na(new_dat$subject[j])) {
    new_dat$subject[j] <-
      base::unique(new_dat$subject)[!is.na(base::unique(new_dat$subject))]
  }
  if (is.na(new_dat$gaps_filled.y[j])) {
    new_dat$gaps_filled.y[j] <- "Yes"
  }
  if (is.na(new_dat$traj_id[j])) {
    new_dat$traj_id[j] <-
      base::unique(new_dat$traj_id)[!is.na(base::unique(new_dat$traj_id))]
  }
  if (is.na(new_dat$file_sub_traj[j])) {
    new_dat$file_sub_traj[j] <-
      base::unique(new_dat$file_sub_traj)[!is.na(base::unique(
        new_dat$file_sub_traj))]
  }
  if (is.na(new_dat$traj_length[j])) {
    new_dat$traj_length[j] <-
      base::unique(new_dat$traj_length)[!is.na(base::unique(
        new_dat$traj_length))]
  }
  if (is.na(new_dat$start_length[j])) {
    new_dat$start_length[j] <-
      base::unique(new_dat$start_length)[!is.na(base::unique(
        new_dat$start_length))]
  }
  if (is.na(new_dat$end_length[j])) {
    new_dat$end_length[j] <-
      base::unique(new_dat$end_length)[!is.na(base::unique(
        new_dat$end_length))]
  }
  if (is.na(new_dat$length_diff[j])) {
    new_dat$length_diff[j] <-
      base::unique(new_dat$length_diff)[!is.na(base::unique(
        new_dat$length_diff))]
  }
  if (is.na(new_dat$start_length_sign[j])) {
    new_dat$start_length_sign[j] <-
      base::unique(new_dat$start_length_sign)[!is.na(base::unique(
        new_dat$start_length_sign))]
  }
  if (is.na(new_dat$end_length_sign[j])) {
    new_dat$end_length_sign[j] <-
      base::unique(new_dat$end_length_sign)[!is.na(base::unique(
        new_dat$end_length_sign))]
  }
  if (is.na(new_dat$direction[j])) {
    new_dat$direction[j] <-
      base::unique(new_dat$direction)[!is.na(base::unique(
        new_dat$direction))]
  }
}

      ## Copy over predicted position and velocity data into the
      ## original data frame
      ## Basically overwrite any NA rows
      for (k in seq_len(nrow(new_dat))) {
        if (is.na(new_dat$position_length.y[k])) {
          new_dat$position_length.y[k] <- new_dat$position_length.x[k]
        }
        if (is.na(new_dat$position_width.y[k])) {
          new_dat$position_width.y[k] <- new_dat$position_width.x[k]
        }
        if (is.na(new_dat$position_height.y[k])) {
          new_dat$position_height.y[k] <- new_dat$position_height.x[k]
        }
        if (is.na(new_dat$velocity.y[k])) {
          new_dat$velocity.y[k] <- new_dat$velocity.x[k]
        }
        if (is.na(new_dat$length_inst_vel.y[k])) {
          new_dat$length_inst_vel.y[k] <- new_dat$length_inst_vel.x[k]
        }
        if (is.na(new_dat$width_inst_vel.y[k])) {
          new_dat$width_inst_vel.y[k] <- new_dat$width_inst_vel.x[k]
        }
        if (is.na(new_dat$height_inst_vel.y[k])) {
          new_dat$height_inst_vel.y[k] <- new_dat$height_inst_vel.x[k]
        }
      }

      ## Keep only the columns we want
      obj_splits[[i]] <-
        new_dat %>%
        ## position_axis.y have the position values we want; position_axis.x
        ## and time_sec.y can be discarded
        dplyr::select(
          frame, time_sec.x, subject,
          position_length.y, position_width.y, position_height.y,
          rotation_length, rotation_width, rotation_height, rotation_real,
          mean_marker_error,
          velocity.y, length_inst_vel.y, width_inst_vel.y, height_inst_vel.y,
          traj_id, file_sub_traj, traj_length, start_length, end_length,
          length_diff, start_length_sign, end_length_sign, direction,
          gaps_filled.y) %>%
        dplyr::rename(
          time_sec = time_sec.x,
          position_length = position_length.y,
          position_width = position_width.y,
          position_height = position_height.y,
          velocity = velocity.y,
          length_inst_vel = length_inst_vel.y,
          width_inst_vel = width_inst_vel.y,
          height_inst_vel = height_inst_vel.y,
          gaps_filled = gaps_filled.y)

    }

    if(traj_gap_indicator[i] == FALSE){
      ## For trajectories that do not need smoothing
      ## Indicate this trajectory was not smoothed
      obj_splits[[i]] <-
        obj_splits[[i]] %>%
        tibble::add_column(gaps_filled = "No")
    }

  }

  ## Put it all back together again
  obj_new <- dplyr::bind_rows(obj_splits)

  ## Leave a note that we smoothed some trajectories
  attr(obj_new,"pathviewR_steps") <-
    c(attr(obj_name,"pathviewR_steps"), "traj_gaps_filled")

  ## Export
  return(obj_new)

}


#################### rm subjects by trajectory number ##########################

#'Remove subjects by trajectory number
#'
#'Specify a minimum number of trajectories that each subject must complete
#'during a treatment, trial, or session.
#'
#'@param obj_name The input viewr object; a tibble or data.frame with attribute
#'  \code{pathviewR_steps} that includes \code{"viewr"}. Trajectories must be
#'  predefined (i.e. via \code{separate_trajectories()}).
#'@param trajnum Minimum number of trajectories; must be numeric.
#'@param mirrored Does the data have mirrored treatments? If so, arguments
#'  \code{treatment1} and \code{treatment2} must also be provided, indicating
#'  the names of two mirrored treatments, both of which must meet the trajectory
#'  threshold specified in \code{trajnum}. Default is FALSE.
#'@param treatment1 The first treatment or session during which the threshold
#'  must be met.
#'@param treatment2 A second treatment or session during which the threshold
#'  must be met.
#'@param ... Additional arguments passed to/from other pathviewR functions.
#'
#'@details Depending on analysis needs, users may want to remove subjects that
#'  have not completed a certain number of trajectories during a treatment,
#'  trial, or session. If \code{mirrored = FALSE}, no treatment information is
#'  necessary and subjects will be removed based on total number of trajectories
#'  as specified in \code{trajnum}. If \code{mirrored = TRUE}, the
#'  \code{treatment1} and \code{treatment2} parameters will allow users to
#'  define during which treatments or sessions subjects must reach threshold as
#'  specified in the \code{trajnum} argument. For example, if \code{mirrored =
#'  TRUE}, setting \code{treatment1 = "latA"}, \code{treatment2 = "latB"} and
#'  \code{trajnum = 5} will remove subjects that have fewer than 5 trajectories
#'  during the \code{"latA"} treatment AND the \code{"latB"} treatment.
#'  \code{treatment1} and \code{treatment2} should be levels within a column
#'  named \code{"treatment"}.
#'
#'@return A viewr object; a tibble or data.frame with attribute
#'  \code{pathviewR_steps} that includes \code{"viewr"} that now has fewer
#'  observations (rows) as a result of removal of subjects with too few
#'  trajectories according to the \code{trajnum} parameter.
#'
#'@export
#'
#'@author Melissa S. Armstrong
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                               package = 'pathviewR'))
#'
#' ## Clean, isolate, and label trajectories
#' motive_full <-
#'   motive_data %>%
#'   clean_viewr(desired_percent = 50,
#'               max_frame_gap = "autodetect",
#'               span = 0.95)
#'
#' ##Remove subjects that have not completed at least 150 trajectories:
#' motive_rm_unmirrored <-
#'   motive_full %>%
#'   rm_by_trajnum(trajnum = 150)
#'
#' ## Add treatment information
#' motive_full$treatment <- c(rep("latA", 100),
#'                            rep("latB", 100),
#'                            rep("latA", 100),
#'                            rep("latB", 149))
#'
#' ## Remove subjects by that have not completed at least 10 trajectories in
#' ## both treatments
#' motive_rm_mirrored <-
#'   motive_full %>%
#'   rm_by_trajnum(
#'     trajnum = 10,
#'     mirrored = TRUE,
#'     treatment1 = "latA",
#'     treatment2 = "latB"
#'   )

rm_by_trajnum <- function(obj_name,
                          trajnum = 5,
                          mirrored = FALSE,
                          treatment1,
                          treatment2,
                          ...) {

  #no treatment:
  if (mirrored == FALSE){
    rm_bytraj <-
      obj_name %>%
      dplyr::group_by(subject) %>%
      tidyr::nest() %>%
      dplyr::mutate(n = data %>%
                      purrr::map_dbl(~ length(.$file_sub_traj))) %>%
      dplyr::select(subject, n)

    bloob <-
      rm_bytraj %>%
      dplyr::filter(n >= trajnum)

    obj_name <-
      dplyr::inner_join(obj_name, bloob)

    return(obj_name)

  }

  #for mirrored treatments
  if (mirrored == TRUE){

    #get list of subjects that complete x num trajectories in BOTH treatments
    rm_bytraj <-
      obj_name %>%
      tidyr::unite(block, "subject", "treatment", sep = "_") %>%
      dplyr::group_by(block) %>%
      tidyr::nest() %>%
      dplyr::mutate(n = data %>%
                      purrr::map_dbl( ~ length(.$file_sub_traj))) %>%
      dplyr::select(block, n) %>%
      tidyr::separate(block, c("subject", "treatment")) %>%
      tidyr::pivot_wider(
        names_from = treatment,
        values_from = n,
        values_fill = 0
      )

    vars <- c(treatment1, treatment2)

    rm_bytraj <-
      rm_bytraj %>%
      dplyr::filter(.data[[vars[[1]]]] >= trajnum &
                      .data[[vars[[2]]]] >= trajnum) %>%
      dplyr::select(subject)

    obj_name <-
      dplyr::inner_join(obj_name, rm_bytraj)

    return(obj_name)
  }

  #if trajnum is character instead of numeric:
  if (is.character(trajnum)) {
    stop("trajnum is character.
    Please check that you have entered the trajnum as a numeric.")
  }

}

###########################    insert_treatments    ############################
#' Inserts treatment and experiment information
#'
#' Adds information about treatment and experimental set up to viewr objects for
#' analysis in other pathviewR functions
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param tunnel_config The configuration of the experimental tunnel.
#' Currently, pathviewR supports rectangular "box" and V-shaped tunnel
#' configurations.
#' @param perch_2_vertex If using a V-shaped tunnel, this is the vertical
#' distance between the vertex and the height of the perches. If the tunnel does
#' not have perches, insert the vertical distance between the vertex and the
#' height of the origin (0,0,0).
#' @param vertex_angle  If using a V-shaped tunnel, the angle of the vertex (in
#' degrees) \code{vertex_angle} defaults to 90.
#' @param tunnel_width If using a box-shaped tunnel, the width of the tunnel.
#' @param tunnel_length The length of the tunnel.
#' @param stim_param_lat_pos The size of the stimulus on the lateral positive
#' wall of the tunnel. Eg. for 10cm wide gratings,
#' \code{stim_param_lat_pos} = 0.1.
#' @param stim_param_lat_neg The size of the stimulus on the lateral negative
#' wall of the tunnel..
#' @param stim_param_end_pos The size of the stimulus on the end positive
#' wall of the tunnel.
#' @param stim_param_end_neg The size of the stimulus on the end negative
#' wall of the tunnel.
#' @param treatment The name of the treatment assigned to all rows of the viewr
#' object. Currently only able to accept a single treatment per viewr data
#' object.
#'
#' @return A viewr object (tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"treatments added"}). Depending
#'   on the argument \code{tunnel_config}, the viewr object also includes
#'   columns storing the values of the supplied arguments. This experimental
#'   information is also stored in the viewr object's metadata
#'
#' @details All length measurements reported in meters.
#'
#' @author Eric R. Press
#'
#' @family utility functions
#'
#' @examples
#'  ## Import sample data from package
#'  motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                               package = 'pathviewR'))
#'
#'   ## Clean data up to and including get_full_trajectories()
#' motive_data_full <-
#'  motive_data %>%
#'  relabel_viewr_axes() %>%
#'  gather_tunnel_data() %>%
#'  trim_tunnel_outliers() %>%
#'  rotate_tunnel() %>%
#'  get_velocity() %>%
#'  select_x_percent(desired_percent = 50) %>%
#'  separate_trajectories(max_frame_gap = "autodetect") %>%
#'  get_full_trajectories(span = 0.95)
#'
#' ## Now add information about the experimental configuration. In this example,
#' ## a V-shaped tunnel in which the vertex is 90deg and lies 0.40m below the
#' ## origin. The visual stimuli on the lateral and end walls have a cycle
#' length of 0.1m and 0.3m respectively, and the treatment is labeled
#' "lat10_end30"
#'
#' motive_v <-
#' motive_data_full %>%
#'  insert_treatments(tunnel_config = "v",
#'                    perch_2_vertex = 0.4,
#'                    vertex_angle = 90,
#'                    tunnel_length = 2,
#'                    stim_param_lat_pos = 0.1,
#'                    stim_param_lat_neg = 0.1,
#'                    stim_param_end_pos = 0.3,
#'                    stim_param_end_neg = 0.3,
#'                    treatment = "lat10_end_30")
#'
#' ## For an experiment using the box-shaped configuration where the tunnel is
#' 1m wide and 3m long and the visual stimuli on the lateral and end walls have
#' a cycle length of 0.2 and 0.3m, respectively, and the treatment is labeled
#' "lat20_end30".
#' flydra_treat <-
#'  flydra_full %>%
#'  insert_treatments(tunnel_config = "box",
#'                    tunnel_width = 1,
#'                    tunnel_length = 3,
#'                    stim_param_lat_pos = 0.2,
#'                    stim_param_lat_neg = 0.2,
#'                    stim_param_end_pos = 0.3,
#'                    stim_param_end_neg = 0.3,
#'                    treatment = "lat20_end30")


insert_treatments <- function(obj_name,
                              tunnel_config = "box",
                              perch_2_vertex = NULL,
                              vertex_angle = 90,
                              tunnel_width = NULL,
                              tunnel_length = NULL,
                              stim_param_lat_pos = NULL,
                              stim_param_lat_neg = NULL,
                              stim_param_end_pos = NULL,
                              stim_param_end_neg = NULL,
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
  ##
  ## figure out how to do it such that only the arguments supplied are added to
  ## the data frame
  if (tunnel_config == "v"){
    obj_name <- tibble::add_column(obj_name, .before = "frame",
                                   tunnel_config = tunnel_config,
                                   perch_2_vertex = perch_2_vertex,
                                   vertex_angle = deg_2_rad(vertex_angle/2),
                                   tunnel_length = tunnel_length,
                                   stim_param_lat_pos = stim_param_lat_pos,
                                   stim_param_lat_neg = stim_param_lat_neg,
                                   stim_param_end_pos = stim_param_end_pos,
                                   stim_param_end_neg = stim_param_end_neg,
                                   treatment = treatment)
  } else if (tunnel_config == "box"){
    obj_name <- tibble::add_column(obj_name, .before = "frame",
                                   tunnel_config = tunnel_config,
                                   tunnel_width = tunnel_width,
                                   tunnel_length= tunnel_length,
                                   stim_param_lat_pos = stim_param_lat_pos,
                                   stim_param_lat_neg = stim_param_lat_neg,
                                   stim_param_end_pos = stim_param_end_pos,
                                   stim_param_end_neg = stim_param_end_neg,
                                   treatment = treatment)
  }

  ## Add arguments into metadata......surewhynot
  if (tunnel_config == "v"){
    attr(obj_name, "tunnel_config") <- tunnel_config
    attr(obj_name, "perch_2_vertex") <- perch_2_vertex
    attr(obj_name, "vertex_angle") <- vertex_angle
    attr(obj_name, "tunnel_width") <- tunnel_width
    attr(obj_name, "tunnel_length") <- tunnel_length
    attr(obj_name, "stim_param_lat_pos") <- stim_param_lat_pos
    attr(obj_name, "stim_param_lat_neg") <- stim_param_lat_neg
    attr(obj_name, "stim_param_end_pos") <- stim_param_end_pos
    attr(obj_name, "stim_param_end_neg") <- stim_param_end_neg
    attr(obj_name, "treatment") <- treatment
  } else if (tunnel_config == "box"){
    attr(obj_name, "tunnel_config") <- tunnel_config
    attr(obj_name, "tunnel_width") <- tunnel_width
    attr(obj_name, "tunnel_length") <- tunnel_length
    attr(obj_name, "stim_param_lat_pos") <- stim_param_lat_pos
    attr(obj_name, "stim_param_lat_neg") <- stim_param_lat_neg
    attr(obj_name, "stim_param_end_pos") <- stim_param_end_pos
    attr(obj_name, "stim_param_end_neg") <- stim_param_end_neg
    attr(obj_name, "treatment") <- treatment
  }

  ## Leave note that treatments were added
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "treatments_added")
  return(obj_name)
}






