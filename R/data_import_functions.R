## Part of the pathviewR package
## Last updated: 2020-09-11 VBB


############################### read_motive_csv ################################
#' Import data from a CSV exported from Optitrack's Motive software
#'
#' \code{read_motive_csv()} is designed to import data from a CSV that has been
#' exported from Optitrack's Motive software. The resultant object is a tibble
#' that additionally has important metadata stored as attributes (see Details).
#'
#' @param file_name A file (or path to file) in CSV format
#' @param file_id (Optional) identifier for this file. If not supplied, this
#' defaults to \code{basename(file_name)}.
#' @param simplify_marker_naming If Markers are encountered, should they be
#' renamed from "Subject:marker" to "marker"? Defaults to TRUE
#' @param ... Additional arguments passed from other \code{pathviewR} functions
#'
#' @details Uses \code{data.table::fread()} to import data from a CSV file and
#' ultimately store it in a tibble. This object is also labeled with the
#' attribute \code{pathviewR_steps} with value \code{viewr} to indicate that it
#' has been imported by \code{pathviewR} and should be friendly towards use with
#' other functions in our package. Additionally, the following metadata are
#' stored in the tibble's attributes: header information from the Motive CSV
#' file (\code{header}), original IDs for each object (\code{Motive_IDs}), the
#' name of each subject in each data column (\code{subject_names_full}) and
#' unique values of subject names (\code{subject_names_simple}), the type of
#' data (rigid body or marker) that appears in each column
#' (\code{data_types_full}) and overall (\code{data_types_simple}), and original
#' data column names in the CSV (\code{d1, d2}). See Example below for example
#' code to inspect attributes.
#'
#' @section Warning:
#' This function was written to read CSVs exported using Motive's Format Version
#' 1.23 and is not guaranteed to work with those from other versions. Please
#' file an Issue on our Github page if you encounter any problems.
#'
#' @return A tibble with numerical data in columns. The first two columns will
#' have frame numbers and time (assumed to be in secs), respectively. Columns 3
#' and beyond will contain the numerical data on the position or rotation of
#' rigid bodies and/or markers that appear in the Motive CSV file. Each row
#' corresponds to the position or rotation of all objects at a given time
#' (frame).
#'
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @family data import functions
#' @seealso \code{\link{read_flydra_mat}} for importing Flydra data
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
#'                              package = 'pathviewR'))
#'
#' ## Names of variables in the resulting tibble
#' names(motive_data)
#'
#' ## A variety of metadata are stored as attributes. Of particular interest:
#' attr(motive_data, "pathviewR_steps")
#' attr(motive_data, "file_id")
#' attr(motive_data, "file_mtime")
#' attr(motive_data, "header")
#' attr(motive_data, "Motive_IDs")
#' attr(motive_data, "subject_names_full")
#' attr(motive_data, "subject_names_simple")
#' attr(motive_data, "motive_data_names")
#' attr(motive_data, "motive_data_types_full")
#' attr(motive_data, "motive_data_types_simple")
#'
#' ## Of course, all attributes can be viewed as a (long) list via:
#' attributes(motive_data)
#'

read_motive_csv <-
  function(file_name,
           file_id = NA,
           simplify_marker_naming = TRUE,
           ...){

    ## Import checks
      if (missing(file_name))
        stop("A file_name is required")
      # if (!file.exists(file_name))
      #   stop(paste0("File ", file_name, " not found!"))

    ## Open connection to file for reading in text mode
      file_con <- file(file_name, "r")

    ## Match file_id to file_name if no file_id is supplied
      if (is.na(file_id)) file_id <- basename(file_name)

    ## Get maketime of file (may not be accurate...use with caution!)
      mtime <- file.info(file_name)$mtime

    ## Setup for reading in file
      header <- c()
      names_line <- c()
      data_names_part_one <- c()
      data_names_part_two <- c()
      data_names <- c()

    ## Read in header; usually just line 1 but grepl() makes this flexbile
      while(!grepl(",Type", (l <- readLines(file_con, 1)))){
        header <- c(header, l)
      }
    ## Split the character vector and make a data.frame of it
      header <-
        ## Chopping out the last entry of header via `length(header)-1` because
        ## the final line is (usually) empty. Revise here if we see errors!!!
        strsplit(header[[1]], ",")[[length(header)-1]] %>%
        data.frame(stringsAsFactors = FALSE)
    ## Convert the 1-col data frame into a 2-col data frame:
      odds <- base::seq_along(header$.) %% 2 == 1
      metadata <- header$.[odds] # odd-numbered rows are metadata
      value <- header$.[!odds] # even-numbered rows are corresponding values
      header <- data.frame(metadata, value,
                           stringsAsFactors = FALSE)

    ## Warn about Motive version
    if (!value[1] == "1.23"){
      warning(
        "This function was written to read CSVs exported using Motive's
Format Version 1.23 and is not guaranteed to work with those from other
versions. Please file an Issue on our Github page if you encounter any
problems.",
        call. = FALSE)
    }

    ## The next line (already read and stored into `l`) should contain info
    ## on the type of data in each column (hopefully either "Rigid Body" or
    ## "Marker")
      type_line <- l
      type_vec <- strsplit(type_line[1], ",")[[1]]
      types <- type_vec[-c(1, 2)]

#### NOTE!!
  ## Add a check here for Types. They must be either "Rigid Body" or
  ## "Marker". If another type is found, the function needs to stop
  ## and then tell the user that a non-Rigid Body non-Marker type
  ## was encountered and our software is not designed to handle these
  ## cases.

    ## Rigid body & marker names (line 4)
      while(!grepl(",ID", (l <- readLines(file_con, 1)))){
        names_line <- c(names_line, l)
      }
    ## Save the line itself for use later
      names_vec <- strsplit(names_line[1], ",")[[1]]
    ## Make a list of the unique rigid bodies and markers
      names <- base::unique(names_vec[-c(1, 2)])
    ## The ID line (typically line 5) is stored in `l` currently
      id_line <- l
      id_vec <- strsplit(id_line[1], ",")[[1]]
      ids <- id_vec[-c(1, 2)]

    ## Create a data.frame that lays out object IDs, types, and names
      ids_types_names <- data.frame(id_vec,
                                    type_vec,
                                    names_vec,
                                    simplify = NA, # container to be filled
                                    subjects = NA  # container to be filled
                                    )

    ## Now add the actual markers' names to the "simplify" column of
    ## ids_types_names. I'm sure this can be re-written to avoid a
    ## for() loop, but I'm keeping it like this for now because it
    ## makes sense to me.
      for (i in 1:nrow(ids_types_names)){
        ## If i-th entry of $type_vec is "Marker"
        base::ifelse(ids_types_names$type_vec[i] == "Marker",
        ## Then place the marker name in $simplify
                     ids_types_names$simplify[i] <- stringr::str_split(
                       ids_types_names$names_vec[i], ":")[[1]][2], # 2nd half
        ## Otherwise, copy the contents of $names_vec
                     ids_types_names$simplify[i] <-
                       ids_types_names$names_vec[i]
                     )
      }

    ## Similarly, use the subject ID in subjects column
      for (i in 1:nrow(ids_types_names)){
        ## If i-th entry of $type_vec is not a "Rigid Body
        base::ifelse(ids_types_names$type_vec[i] != "Rigid Body",
        ## Then place the marker name in $subjects
                     ids_types_names$subjects[i] <- stringr::str_split(
                       ids_types_names$names_vec[i], ":")[[1]][1], # 1st half
        ## Otherwise, copy the contents of $names_vec
                     ids_types_names$subjects[i] <-
                       ids_types_names$names_vec[i]
        )
      }

    ## Data names line 6
    l <- readLines(file_con, 1)
    data_names_part_one <- strsplit(l, ",")[[1]]

    ## Data names line 7
    l <- readLines(file_con, 1)
    data_names_part_two <- strsplit(l, ",")[[1]]

    ## Make variables same length
    ## Often arises when final column is "Marker Quality" in line 6 with
    ## nothing in line 7
    if (length(data_names_part_two) == length(data_names_part_one) - 1) {
      data_names_part_two <- c(data_names_part_two, "") # add blank character
    } else {
      data_names_part_two <- data_names_part_two # keep the same
    }

    ## Handle choice of simplify_marker_naming = TRUE or FALSE
    if (simplify_marker_naming == TRUE){
      marker_id_line <- ids_types_names$simplify
    } else {
      marker_id_line <- names_vec
    }

    ## Make data frame to help with data names
    data_names_frame <- data.frame(v1 = marker_id_line,
                                   v2 = data_names_part_one,
                                   v3 = data_names_part_two)
    ## v2 (Position) seems useless, but I'm keeping it in for now and ignoring
    ## it in the next step

    ## Stitch it together
    for (i in 1:nrow(data_names_frame)) {
      data_names[[i]] <- paste0(data_names_frame[i, 1],
                                "_",
                                data_names_frame[i, 2],
                                "_",
                                data_names_frame[i, 3])
    }

    ## Some hacky cleanup
    ## Not ideal but it works
    data_names <- sub("__Frame", "frame", data_names,
                      ignore.case = FALSE)
    data_names <- sub(" ", "_", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Name__Time_\\(Seconds\\)", "time_sec", data_names,
                      ignore.case = FALSE)
    ## We'd like to snake_case-ify names of variables, but I currently am
    ## averse to altering the names of subjects, too. So I will opt to
    ## be inconsistent here re:converting to lower case for the moment.
    ## Should we opt to extend snake_case-ing to subjects, I expect
    ## most of what follows can be written with code that's more efficient
    data_names <- sub("Mean Marker Error_", "mean_marker_error", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Mean_Marker Error_", "mean_marker_error", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Position_X", "position_x", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Position_Y", "position_y", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Position_Z", "position_z", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Rotation_X", "rotation_x", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Rotation_Y", "rotation_y", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Rotation_Z", "rotation_z", data_names,
                      ignore.case = FALSE)
    data_names <- sub("Rotation_W", "rotation_w", data_names,
                      ignore.case = FALSE)


    ## Read in data
    dataz <- data.table::fread(
      text = readLines(file_con),
      header = FALSE,
      sep = ",",
      dec = ".",
      stringsAsFactors = FALSE
    )
    ## Quickly check for non-numerics and warn if found
    if (any(!apply(dataz, 2, is.numeric))) {
      warning("The csv file includes non-numeric data.
              \nPlease ensure that this is intentional before proceeding.")
    }

    ## Rename columns
    colnames(dataz) <- data_names

    ## Be neat and close up the file connection
    close(file_con)

    ## Make the object (a tibble)
    data <- tibble::as_tibble(dataz)

    ## Add metadata as attributes()
    attr(data, "pathviewR_steps") <- "viewr"
    attr(data, "file_id") <- file_id
    attr(data, "file_mtime") <- mtime
    attr(data, "frame_rate") <- header$value[5] %>% as.numeric()
    attr(data, "header") <- header
    attr(data, "Motive_IDs") <- ids
    attr(data, "subject_names_full") <- names_vec[-c(1,2)]
    attr(data, "subject_names_simple") <- names
    attr(data, "data_names") <- data_names
    attr(data, "data_types_full") <- types
    attr(data, "data_types_simple") <- base::unique(types)
    attr(data, "d1") <- data_names_part_one
    attr(data, "d2") <- data_names_part_two
    attr(data, "import_method") <- "motive"

    ## Export
    return(data)
  }


################################# read_flydra_mat #############################
## Time is now encoded as a function of frame_rate and the specific labeling
## of frames within the imported flydra object

#' Import data from a MAT file exported from Flydra software
#'
#' \code{read_flydra_mat()} is designed to import data from a \code{.mat} file
#' that has been exported from Flydra software. The resultant object is a tibble
#' that additionally has important metadata stored as attributes (see Details).
#'
#' @param mat_file A file (or path to file) in .mat format, exported from Flydra
#' @param file_id (Optional) identifier for this file. If not supplied, this
#' defaults to \code{basename(file_name)}.
#' @param subject_name Name that will be assigned to the subject
#' @param frame_rate The capture frame rate of the session
#' @param ... Additional arguments that may be passed from other pathviewR
#'   functions
#'
#' @return A tibble with numerical data in columns. The first two columns will
#'   have frame numbers and time (assumed to be in secs), respectively. Columns
#'   3 through 5 will contain position data. Note that unlike the behavior of
#'   \code{read_motive_csv()} this function produces "tidy" data that have
#'   already been gathered into key-value pairs based on subject.
#'
#' @author Vikram B. Baliga
#'
#' @family data import functions
#'
#' @seealso \code{\link{read_motive_csv}} for importing Motive data
#'
#' @export
#' @examples
#' library(pathviewR)
#'
#' ## Import the example Flydra data included in the package
#' flydra_data <-
#'   read_flydra_mat(system.file("extdata", "pathviewR_flydra_example_data.mat",
#'                              package = 'pathviewR'),
#'                   subject_name = "birdie_wooster")
#'
#' ## Names of variables in the resulting tibble
#' names(flydra_data)
#'
#' ## A variety of metadata are stored as attributes. Of particular interest:
#' attr(flydra_data, "pathviewR_steps")

read_flydra_mat <-
  function(mat_file,
           file_id = NA,
           subject_name,
           frame_rate = 100, ## in Hz
           ...) {

    ## Import checks
    if (missing(mat_file))
      stop("A mat_file is required")
    # if (!file.exists(mat_file))
    #   stop(paste0("File ", mat_file, " not found!"))

    ## For now, we will assume that only one subject (one individual
    ## hummingbird) is present in the data. Since these subject names were not
    ## stored in the flydra data or accompanying H5 files (as far as I can see)
    ## this will need to be supplied by the user
    if (missing(subject_name))
      stop("A subject_name is required")

    ## Match file_id to mat_file if no file_id is supplied
    if (is.na(file_id)) file_id <-
        basename(mat_file)

    ## Get maketime of file (may not be accurate...use with caution!)
    mtime <-
      file.info(mat_file)$mtime

    ## Read the MAT file via R.matlab::readMat()
    mat_read <-
      R.matlab::readMat(mat_file)

    ## The data we'd like to tibble-ize is spread across various components
    ## of the list. We need to put it together manually.

    ## First extract the kalmanized componenets
    kalm_tib <-
      tibble::tibble(
        kalman.frame = mat_read$kalman.frame,
        kalman.x = mat_read$kalman.x,
        kalman.y = mat_read$kalman.y,
        kalman.z = mat_read$kalman.z
      ) %>%
      ## For some reason, frames may be out of order
      ## So sort this tibble by kalman.frame
      dplyr::arrange(by = kalman.frame)

    ## There may be duplication of frames or rows entirely.
    ## These duplications should be removed via distinct()
    kalm_distinct <-
      dplyr::distinct(kalm_tib, NAME = kalman.frame, .keep_all = TRUE)

    ## First get the dimensions of the data
    data_length <- length(mat_read$kalman.y)

    ## Now extract all the frames
    framez <- kalm_distinct$kalman.frame
    frame_tib <- tibble::tibble(frame = framez)
    ## Because frames may have been dropped, we will first generate a
    ## full sequence that begins and ends on the min and max frames.
    ## Then we'll generate time stamps that correspond to those frames.
    ## Finally, we'll drop time stamps for any frames that were dropped
    ## in the exported matlab file.
    frame_first <- min(framez)
    frame_last <- max(framez)
    frame_seq <- seq(from = frame_first, to = frame_last, by = 1)
    time_interval <- 1/frame_rate
    ## We'll set time to start at 0 and then increase by time_interval until
    ## it hits the same vector length as length(frame_seq)
    time_seq <- seq(from = 0, by = time_interval,
                    length.out = length(frame_seq))
    ## Now combine
    frame_time_seqs <- tibble::tibble(frame = frame_seq,
                                      time  = time_seq)
    ## Need to join it back to frame_tib in case frame ordering shifts
    ## or is duplicated within the data
    joined_frame_time_seq <- dplyr::left_join(frame_tib, frame_time_seqs,
                                              by = "frame")

    ## Now put the data together
    data <-
      tibble::tibble(
        ## Using kalman frame instead of observed frame
        frame = frame_tib$frame,
        time_sec = joined_frame_time_seq$time,
        subject = subject_name,
        position_length = kalm_distinct$kalman.x,
        position_width  = kalm_distinct$kalman.y,
        position_height = kalm_distinct$kalman.z
      )

    ## Add metadata as attributes()
    attr(data, "pathviewR_steps") <-
      c("viewr", "renamed_tunnel", "gathered_tunnel")
    ## Adding "renamed_tunnel" and "gathered" because axes are renamed as the
    ## tibble is being created above and we are basically already in gathered
    ## format.
    attr(data, "file_id") <- file_id
    attr(data, "file_mtime") <- mtime
    attr(data, "frame_rate") <- frame_rate
    ## Re-enable the following line in the future if you would like to store
    ## the original matlab file as an attribute:
    # attr(data, "flydra_mat") <- mat_read
    attr(data, "header") <- attr(mat_read, "header")
    attr(data, "import_method") <- "flydra"

    ## Export
    return(data)
  }


################################### as_viewr ###################################

#' Convert data from another format into a viewr object
#'
#' Should you have data from a non-Motive, non-Flydra source, this function can
#' be used to ensure your data are put into the right format to work with other
#' pathviewR functions.
#'
#' @param obj_name A tibble or data frame containing movement trajectories
#' @param frame_rate Must be a single numeric value indicating capture frame
#'   rate in frames per second.
#' @param frame_col Column number of obj_name that contains frame numbers
#' @param time_col Column number of obj_name that contains time (must be in
#'   seconds)
#' @param subject_col Column number of obj_name that contains subject name(s)
#' @param position_length_col Column number of obj_name that contains
#'   length-axis position values
#' @param position_width_col Column number of obj_name that contains width-axis
#'   position values
#' @param position_height_col Column number of obj_name that contains
#'   height-axis position values
#' @param include_rotation Are rotation data included? Defaults to FALSE
#' @param rotation_real_col Column number of obj_name that contains the "real"
#'   axis of quaternion rotation data
#' @param rotation_length_col Column number of obj_name that contains the length
#'   axis of quaternion rotation data
#' @param rotation_width_col Column number of obj_name that contains the width
#'   axis of quaternion rotation data
#' @param rotation_height_col Column number of obj_name that contains the height
#'   axis of quaternion rotation data
#'
#' @return A tibble that is organized to be compliant with other
#'   \code{pathviewR} functions and that contains the attributes
#'   \code{pathviewR_steps} with entries set to \code{c("viewr",
#'   "renamed_tunnel", "gathered_tunnel")}
#'
#' @export
#'
#' @family data import functions
#'
#' @author Vikram B. Baliga
#'
#' @examples
#'
#' ## Create a dummy data frame with simulated (nonsense) data
#' df <- data.frame(frame = seq(1, 100, by = 1),
#'                  time_sec = seq(0, by = 0.01, length.out = 100),
#'                  subject = "birdie_sanders",
#'                  z = rnorm(100),
#'                  x = rnorm(100),
#'                  y = rnorm(100))
#'
#' ## Use as_viewr() to convert it into a viewr object
#' test <-
#'   as_viewr(
#'     df,
#'     frame_rate = 100,
#'     frame_col = 1,
#'     time_col = 2,
#'     subject_col = 3,
#'     position_length_col = 5,
#'     position_width_col = 6,
#'     position_height_col = 4
#'   )

as_viewr <- function(obj_name,
                     frame_rate = 100,
                     frame_col,
                     time_col,
                     subject_col,
                     position_length_col,
                     position_width_col,
                     position_height_col,
                     include_rotation = FALSE,
                     rotation_real_col,
                     rotation_length_col,
                     rotation_width_col,
                     rotation_height_col
                     ){

  ## Check that obj_name is a tibble or data.frame
  if (!tibble::is_tibble(obj_name))
    if (!is.data.frame(obj_name))
    stop("A tibble or data.frame must be supplied to the obj_name argument.")

  ## Extract each variable
  data <- data.frame(
    frame           = obj_name[, frame_col],
    time_sec        = obj_name[, time_col],
    subject         = obj_name[, subject_col],
    position_length = obj_name[, position_length_col],
    position_width  = obj_name[, position_width_col],
    position_height = obj_name[, position_height_col]
  )
  colnames(data) <- c("frame", "time_sec", "subject",
                      "position_length", "position_width", "position_height")
  data <- tibble::tibble(data)

  ## Optional arguments, depending on data
  if (include_rotation == TRUE){
    data$rotation_real   <- obj_name[,rotation_real_col]
    data$rotation_length <- obj_name[,rotation_length_col]
    data$rotation_width  <- obj_name[,rotation_width_col]
    data$rotation_height <- obj_name[,rotation_height_col]
    }

  ## Add metadata as attributes()
  attr(data, "pathviewR_steps") <-
    c("viewr", "renamed_tunnel", "gathered_tunnel")
  ## Adding "renamed_tunnel" and "gathered" because axes are renamed as the
  ## tibble is being created above and we are basically already in gathered
  ## format.
  attr(data, "file_id") <- deparse(quote(obj_name))
  attr(data, "frame_rate") <- frame_rate
  attr(data, "import_method") <- "as_viewr"

  return(data)
}
