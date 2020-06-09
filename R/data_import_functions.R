## Part of the pathviewR package
## Last updated: 2020-06-09 VBB


################################# read_motiv_csv ###############################
## Import data from a CSV and create a `motiv` object
## Basically behaves as a tibble but with metadata stored as attributes
## And (currently un-ideal) automatic column naming
## Works for rigid body trials, not sure about others

read_motiv_csv <-
  function(file_name,
           file_id = NA,
           ...){

    ## Import checks
    if (missing(file_name))
      stop("A file_name is required")
    if (!file.exists(file_name))
      stop(paste0("File ", file_name, " not found!"))

    ## Open connection to file for reading in text mode
    file_con <- file(file_name, "r") # means reading in text mode

    ## Match file_id to file_name if no file_id is supplied
    if (is.na(file_id)) file_id <- basename(file_name)

    ## Get maketime of file (may not be accurate...use with caution!)
    mtime <- file.info(file_name)$mtime

    ## Setup for reading in file
    header <- c()
    rigid_bodies <- c()
    rigid_bodies_line <- c()
    data_names_part_one <- c()
    data_names_part_two <- c()
    data_names <- c()

    ## Read in header (line 1)
    header <- c(header, readLines(file_con, 1))
    ## Split the character vector and make a data.frame of it
    header <-
      strsplit(header[[1]], ",")[[1]] %>%
      data.frame(stringsAsFactors = FALSE)
    ## Convert the 1-col data frame into a 2-col data frame:
    odds <- seq_along(header$.) %% 2 == 1
    metadata <- header$.[odds] # odd-numbered rows are metadata
    value <- header$.[!odds] # even-numbered rows are corresponding values
    header <- data.frame(metadata, value,
                         stringsAsFactors = FALSE)

    ## Rigid body IDs
    l <- readLines(file_con, 3)
    ## Save the line itself for use later
    rigid_bodies_line <- strsplit(l[3], ",")[[1]]
    ## Make a list of the unique rigid bodies
    rigid_bodies <- base::unique(rigid_bodies_line[-c(1, 2)])

    ## Data names line 6
    l <- readLines(file_con, 2)
    data_names_part_one <- strsplit(l[2], ",")[[1]]

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

    ## Make data frame to help with data names
    data_names_frame <- data.frame(v1 = rigid_bodies_line,
                                   v2 = data_names_part_one,
                                   v3 = data_names_part_two)

    ## Stitch it together
    ## Using a for() loop for now; I'm sure purrr::map() can be used instead
    for (i in 1:nrow(data_names_frame)) {
      data_names[[i]] <- paste0(data_names_frame[i, 1],
                                "_",
                                data_names_frame[i, 2],
                                "_",
                                data_names_frame[i, 3])
    }

    ## Some hacky cleanup
    ## Not ideal but it works
    data_names <- sub("__Frame", "Frame", data_names)
    data_names <- sub("Name__Time", "Time", data_names)
    data_names <- sub("Marker Error_", "Marker Error", data_names)

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

    ## Close up
    close(file_con)

    ## Make the object (a tibble)
    data <- tibble::as_tibble(dataz)

    ## Add metadata as attributes()
    attr(data,"pathviewR_steps") <- "motiv"
    attr(data,"file_id") <- file_id
    attr(data,"file_mtime") <- mtime
    attr(data,"header") <- header
    attr(data,"rigid_bodies") <- rigid_bodies
    attr(data,"data_names") <- data_names
    attr(data,"d1") <- data_names_part_one
    attr(data,"d2") <- data_names_part_two

    ## Export
    return(data)
  }


############################### read_motive_csv ################################
## Import data from a CSV and create a tibble with a `viewr` attribute
## Basically behaves as a tibble but with metadata stored as attributes

## 2020-06-09: OK I think the function below mostly works. I haven't
## really tested it thoroughly, but in principle it should be able to
## flexibly handle Motive CSVs that have rigid bodies and/or markers
## (including files that mix the two). I've written some comments on
## checks that could be inserted to ensure things are working.

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
    data_names <- sub("__Frame", "frame", data_names)
    data_names <- sub(" ", "_", data_names)
    data_names <- sub("Name__Time_\\(Seconds\\)", "time_sec", data_names)
    data_names <- sub("Mean Marker Error_", "Mean_Marker_Error", data_names)
    data_names <- sub("Mean_Marker Error_", "Mean_Marker_Error", data_names)

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


################################ get_header_motiv ##############################
## Extract header info from imported `motiv` object
get_header_motiv <-function(obj_name,
                            ...) {
  ## Check that it's a motiv object
  if (!any(attr(obj_name,"pathviewR_steps") == "motiv")) {
    stop("This doesn't seem to be a motiv object")
  }

  ## Get the header
  return(attr(obj_name,"header"))
}



##### HERE BE DRAGONS ######
## Code that may be useful once we try to "package-ize" all this
## Basically, it would be good to use grepl() to automate reading the headers
## to intelligently determine how and where information is stored. This will be
## important if headers vary in length for some reason.

# ## Read in Header (line 1)
# while (!grepl(",Type", (l <- readLines(file_con, 1)))) {
#   header <- c(header, l)
# }
# header <-
#   strsplit(header[[1]], ",")[[1]] %>%
#   #matrix(ncol = 2, byrow = TRUE) %>%
#   data.frame(stringsAsFactors = FALSE)
# #colnames(header) <- c("metadata" , "value")
#
# ## Now list rigid body IDs
# while (!grepl(",ID", (l <- readLines(file_con, 1)))) {
#   rigid_bodies <- c(rigid_bodies, l)
# }
# rigid_bodies <- strsplit(rigid_bodies[[1]], ",")[[1]]
# #rigid_bodies <- levels(as.factor(rigid_bodies[-c(1, 2)]))
#
# ## Get names of variables; this will need to happen in multiple steps
# while (!grepl("Frame,", (l <- readLines(file_con, 1)))) {
#   data_names_part_one <- c(rigid_bodies, l)
# }
# ## First line:
# data_names_part_one <- strsplit(data_names_part_one[[3]], ",")[[1]]
# ## Second line:
# data_names_part_two <- strsplit(l, ",")[[1]]
#
# ## Clean up missing parts
# # d_n_o <- sub("^$", "_", data_names_part_one)
# # d_n_t <- sub("^$", "_", data_names_part_two)
# d_n_o <- data_names_part_one
# d_n_t <- data_names_part_two
# d_d_f <- cbind(d_n_o, d_n_t)
#
# data_names <- c()
# for (i in 1:nrow(d_d_f)) {
#   data_names[[i]] <- paste0(d_d_f[i, 1],
#                             "_",
#                             d_d_f[i, 2])
# }
# data_names <- sub("Error_Frame", "Error", data_names)
