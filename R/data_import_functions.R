## Part of the pathviewR package
## Last updated: 2020-06-05 VBB


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

## 2020-06-05: Gonna keep read_motiv_csv() untouched so we have a reliable
## import function for most of our use cases. Will start morphing this one
## into a more generalized Motive import function that will flexibly adapt
## to either rigid body or marker set cases. Until this comment disappears,
## consider the below a "work in progress" and use with caution!

read_motive_csv <-
  function(file_name,
           file_id = NA,
           #data_type = "autodetect",
           ...){

    ## Import checks
      if (missing(file_name))
        stop("A file_name is required")
      if (!file.exists(file_name))
        stop(paste0("File ", file_name, " not found!"))

    ## Check if data_type is one of the known cases
    ## Must be "autodetect", "rigid_body", "marker_set"
    ## Will develop this later

    ## Open connection to file for reading in text mode
      file_con <- file(file_name, "r")

    ## Match file_id to file_name if no file_id is supplied
      if (is.na(file_id)) file_id <- basename(file_name)

    ## Get maketime of file (may not be accurate...use with caution!)
      mtime <- file.info(file_name)$mtime

    ## Setup for reading in file
      header <- c()
      names_line <- c()
      data_id <- c()
      data_id_line <- c()
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
        "pathviewR was built to read CSVs exported using Motive's
Format Version 1.23 and is not guaranteed to work with other versions.
Please file an Issue on our Github page if you encounter any problems",
        call. = FALSE)
    }

    ## The next line (already read and stored into `l`) should contain info
    ## on the type of data in each column (hopefully either "Rigid Body" or
    ## "Marker")
      type_line <- l
      type_vec <- strsplit(type_line[1], ",")[[1]]
      types <- type_vec[-c(1, 2)]

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

### PAUSING HERE FOR NOW. PICK UP FROM THIS POINT NEXT TIME!

    ## Marker IDs
    l <- readLines(file_con, 3)
    ## Save the line itself for use later
    marker_id_line <- strsplit(l[3], ",")[[1]]
    ## Make a list of the unique rigid bodies
    marker_id <- unique(marker_id_line[-c(1, 2)])

    ## Rename Marker IDs and also extract Subject ID
    namez <- marker_id
    ##  Regular expressions are the woooooorst
    subect_id_colon <- stringr::str_extract(namez, ".+?(?<=:)")[1]
    subject_id <- sub(":", "", subject_id_colon)
    marker_id <- sub(subject_id_colon, "", namez)
    marker_id_line <- sub(subject_id_colon, "", marker_id_line)

    ## Might as well add subject_id to header
    subject_df <- data.frame("Subject ID", subject_id)
    names(bird_df) <- c("metadata", "value")
    header <- rbind(header, bird_df)

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
    data_names_frame <- data.frame(v1 = marker_id_line,
                                   v2 = data_names_part_one,
                                   v3 = data_names_part_two)
    ## v2 (Position) seems useless, but I'm keeping it in for now and ignoring
    ## it in the next step

    ## Stitch it together
    ## Using a for() loop for now; I'm sure purrr::map() can be used instead
    ## but who cares
    for (i in 1:nrow(data_names_frame)) {
      data_names[[i]] <- paste0(data_names_frame[i, 1],
                                "_",
                                #data_names_frame[i, 2],
                                #"_",
                                data_names_frame[i, 3])
    }

    ## Some hacky cleanup
    ## Not ideal but it works
    data_names <- sub("_Frame", "frame", data_names)
    data_names <- sub(" ", "_", data_names)
    data_names <- sub("Name_Time_\\(Seconds\\)", "time_sec", data_names)

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
