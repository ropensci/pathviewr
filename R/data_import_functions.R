## Part of the pathviewR package
## Last updated: 2020-06-17 VBB


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
#' @export
#'
#' @author Vikram B. Baliga
#'
#' @family data import functions
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the july 29 example data included in the package
#' jul_29 <-
#'   read_motive_csv(system.file("extdata", "july-29_group-I_16-20.csv",
#'                              package = 'pathviewR'))
#'
#' ## Names of variables in the resulting tibble
#' names(jul_29)
#'
#' ## A variety of metadata are stored as attributes. Of particular interest:
#' attr(jul_29, "pathviewR_steps")
#' attr(jul_29, "file_id")
#' attr(jul_29, "file_mtime")
#' attr(jul_29, "header")
#' attr(jul_29, "Motive_IDs")
#' attr(jul_29, "subject_names_full")
#' attr(jul_29, "subject_names_simple")
#' attr(jul_29, "jul_29_names")
#' attr(jul_29, "jul_29_types_full")
#' attr(jul_29, "jul_29_types_simple")
#'
#' ## Of course, all attributes can be viewed as a (long) list via:
#' attributes(jul_29)
#'
#' @seealso
#' \code{\link{get_header_viewr}} to quickly see header info from the CSV;
#' \code{\link{relabel_viewr_axes}} to rename coordinate axes

read_motive_csv <-
  function(file_name,
           file_id = NA,
           simplify_marker_naming = TRUE,
           ...){

    ## Import checks
      if (missing(file_name))
        stop("A file_name is required")
      if (!file.exists(file_name))
        stop(paste0("File ", file_name, " not found!"))

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
      odds <- seq_along(header$.) %% 2 == 1
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
    attr(data, "header") <- header
    attr(data, "Motive_IDs") <- ids
    attr(data, "subject_names_full") <- names_vec[-c(1,2)]
    attr(data, "subject_names_simple") <- names
    attr(data, "data_names") <- data_names
    attr(data, "data_types_full") <- types
    attr(data, "data_types_simple") <- base::unique(types)
    attr(data, "d1") <- data_names_part_one
    attr(data, "d2") <- data_names_part_two

    ## Export
    return(data)
  }


################################# read_flydra_data #############################
## It is nearly there but I still need to figure out how time (or frame rate)
## is encoded.


read_flydra_data <-
  function(mat_file,
           file_id = NA,
           subject_name,
           ...) {

    ## Import checks
    if (missing(mat_file))
      stop("A mat_file is required")
    if (!file.exists(mat_file))
      stop(paste0("File ", mat_file, " not found!"))

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

    ## First get the dimensions of the data
    data_length <- length(mat_read$kalman.y)

    ## Now put the data together
    data <-
      tibble(
        # using kalman frame instead of observed frame
        frame = mat_read$kalman.frame,
        ## I actally don't know the time intervals yet, so I am just putting
        ## in a dummy sequence.
        time_sec = seq(from = 0, to = (data_length - 1), by = 1),
        subject = subject_name,
        position_length = mat_read$kalman.x,
        position_width = mat_read$kalman.y,
        position_height = mat_read$kalman.z
      )

    ## Add metadata as attributes()
    attr(data, "pathviewR_steps") <-
      c("viewr", "renamed_tunnel", "gathered_tunnel")
    ## Adding "renamed_tunnel" and "gathered" because axes are renamed as the
    ## tibble is being created above and we are basically already in gathered
    ##  format.
    attr(data, "file_id") <- file_id
    attr(data, "file_mtime") <- mtime
    ## We will opt to store the original matlab file as an attribute since
    ## it very likely contains things we may need later. Hard to say what
    ## exactly right now; this is motivated by spidey-sense...
    ## It also doubles the object size -- not very ideal. Fix this soon!
    attr(data, "flydra_mat") <- mat_read
    attr(data, "header") <- attr(mat_read, "header")

    ## Export
    return(data)
  }


################################# get_header_viewr #############################
#' Extract header info from imported viewr object
#'
#' A function to quickly return the information stored in the header of the
#' original data file imported via \code{pathviewR}'s \code{read_} functions.
#'
#' @param obj_name A tibble imported via \code{pathviewR}'s \code{read_}
#' functions with value \code{viewr} appearing in the attribute
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
#' @family data import functions
#'
#' @examples
#' library(pathviewR)
#'
#' ## Import the july 29 example data included in the package
#' jul_29 <-
#'   read_motive_csv(system.file("extdata", "july-29_group-I_16-20.csv",
#'                              package = 'pathviewR'))
#'
#' ## Now display the Header information
#' get_header_viewr(jul_29)
#'
#' @seealso
#' \code{\link{read_motive_csv}} to import data exported from Motive, in CSV
#' format

get_header_viewr <- function(obj_name,
                             ...) {
  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Get the header
  return(attr(obj_name,"header"))
}

