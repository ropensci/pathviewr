## Part of the pathviewR package
## Last updated: 2020-09-05 VBB

################################## get_velocity ################################

#' Get instantaneous velocity for subjects
#'
#' Velocity (both overall and per-axis) is computed for each row in the data
#' (see Details)
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param time_col Name of the column containing time
#' @param length_col Name of the column containing length dimension
#' @param width_col Name of the column containing width dimension
#' @param height_col Name of the column containing height dimension
#' @param add_to_viewr Default TRUE; should velocity data be added as new
#'   columns or should this function create a new simpler object?
#' @param velocity_min Should data below a certain velocity be filtered out of
#'   the object? If so, enter a numeric. If not, keep NA.
#' @param velocity_max Should data above a certain velocity be filtered out of
#'   the object? If so, enter a numeric. If not, keep NA.
#' @param ... Additional arguments passed to or from other pathviewR functions.
#'
#' @details Instantaneous velocity is not truly "instantaneous" but rather is
#' approximated as the change in distance divided by change in time from one
#' observation (row) to the previous observation (row). Each component of
#' velocity is computed (i.e. per axis) along with the overall velocity of
#' the subject.
#'
#' @return If \code{add_to_viewr} is \code{TRUE}, additional columns are
#'   appended to the input viewr object. If \code{FALSE}, a standalone tibble is
#'   created. Either way, an "instantaneous" velocity is computed as the
#'   difference in position divided by the difference in time as each successive
#'   row is encountered. Additionally, velocities along each of the three
#'   position axes are computed and provided as additional columns.
#'
#' @author Vikram B. Baliga and Melissa S. Armstrong
#'
#' @family mathematical functions
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
#' ## "standarization" step before running get_velocity().
#'  motive_cleaned <-
#'    motive_data %>%
#'    relabel_viewr_axes() %>%
#'    gather_tunnel_data() %>%
#'    trim_tunnel_outliers() %>%
#'    rotate_tunnel()
#'
#' ## Now compute velocity and add as columns
#'  motive_velocity_added <-
#'    motive_cleaned %>%
#'    get_velocity(add_to_viewr = TRUE)
#'
#' ## Or set add_to_viewr to FALSE for a standalone object
#'  motive_velocity_standalone <-
#'    motive_cleaned %>%
#'    get_velocity(add_to_viewr = TRUE)

get_velocity <- function(obj_name,
                         time_col = "time_sec",
                         length_col = "position_length",
                         width_col = "position_width",
                         height_col = "position_height",
                         add_to_viewr = TRUE,
                         velocity_min = NA,
                         velocity_max = NA,
                         ...) {

  # ## Check that it's a viewr object
  # if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
  #   stop("This doesn't seem to be a viewr object")
  # }

  ## Argument checks
  if (!any(grepl(time_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("time_col not found.
Please check that you have entered the name of the time variable correctly.")
  }

  if (!any(grepl(length_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("length_col not found.
Please check that you have entered the name of the length variable correctly.")
  }

  if (!any(grepl(width_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("width_col not found.
Please check that you have entered the name of the width variable correctly.")
  }

  if (!any(grepl(height_col,
                 colnames(obj_name),
                 ignore.case = FALSE))) {
    stop("height_col not found.
Please check that you have entered the name of the height variable correctly.")
  }

  ## Convert to data.frame for easier column extraction
  df <- as.data.frame(obj_name)

  ## Create vector of time differences
  time_diffs <- diff(df[,time_col])

  ## Calculate instantaneous velocities
  ## First entry is set to zero because calculating velocity depends on previous
  ## step. We may opt change this to `NA` later on so that these can be filtered
  ## easily.
  length_inst <- c(0, diff(df[,length_col]) / time_diffs)
  width_inst  <- c(0, diff(df[,width_col])  / time_diffs)
  height_inst <- c(0, diff(df[,height_col]) / time_diffs)

  ## Calculate object velocity
  vel <- sqrt((length_inst ^ 2) + (width_inst ^ 2) + (height_inst ^ 2))

  ## Combine
  res <- tibble::tibble(velocity = vel,
                        length_inst_vel = length_inst,
                        width_inst_vel = width_inst,
                        height_inst_vel = height_inst
                        )

  ## If add_to_viewr is TRUE, add it as new columns to the input viewr object.
  ## If add_to_viewr is FALSE, export the standalone (res) tibble
  if (add_to_viewr == TRUE){
    ## Add the new columns and generate a new viewr object
    obj_new <- dplyr::bind_cols(obj_name, res)

    ## Leave a note that we computed velocity via get_velocity()
    attr(obj_new,"pathviewR_steps") <-
      c(attr(obj_name,"pathviewR_steps"), "velocity_computed")

  } else { ## if FALSE
    obj_new <- res
  }

  #let's add some threshold arguments to set biologically reasonable limits
  #velocity_min
  if (is.numeric(velocity_min)) {
    ## filter velocity
    obj_new <- obj_new %>%
      dplyr::filter(velocity > velocity_min)

    ## Leave a note set velocity_min via get_velocity()
    #leave note even if not added to viewr object?
    attr(obj_new, "velocity_min") <- velocity_min

  } else {
    ## if FALSE
    obj_new <- obj_new
    #if is character instead of numeric:
    if (is.character(velocity_min)) {
      stop(
        "velocity_min is character.
    Please check that you have entered the velocity_min variable correctly."
      )
    }
  }
  #velocity_max
  if (is.numeric(velocity_max)) {
    ## filter velocity
    obj_new <- obj_new %>%
      dplyr::filter(velocity < velocity_max)

    ## Leave a note set velocity_min via get_velocity()
    #leave note even if not added to viewr object?
    attr(obj_new, "velocity_max") <- velocity_max

  } else {
    ## if FALSE
    obj_new <- obj_new
    #if is character instead of numeric:
    if (is.character(velocity_max)) {
      stop(
        "velocity_max is character.
    Please check that you have entered the velocity_max variable correctly."
      )
    }
  }

  ## Output
  return(obj_new)

}


############################## get_dist_point_line #############################

#' Compute distance between a point and a line
#'
#' @param point 2D or 3D coordinates of the point as c(x,y) or c(x,y,z)
#' @param line_coord1 2D or 3D coordinates of one point on the line as c(x,y) or
#'   c(x,y,z)
#' @param line_coord2 2D or 3D coordinates of a second point on the line as
#'   c(x,y) or c(x,y,z)
#'
#' @details The function accepts 2D coordinates or 3D coordinates, but note that
#'   the dimensions of all supplied arguments must match; all coordinates must
#'   be 2D or they all must be 3D.
#'
#' @return A numeric vector of length 1 that provides the euclidean distance
#'   between the point and the line.
#'
#' @author Vikram B. Baliga
#'
#' @family mathematical functions
#'
#' @export
#'
#' @examples
#' ## 2D case
#' get_dist_point_line(
#'   point = c(0, 0),
#'   line_coord1 = c(1, 0),
#'   line_coord2 = c(1, 5)
#' )
#'
#' ## 3D case
#' get_dist_point_line(
#'   point = c(0, 0, 0),
#'   line_coord1 = c(1, 0, 0),
#'   line_coord2 = c(1, 5, 0)
#' )

get_dist_point_line <- function(point,
                                line_coord1,
                                line_coord2) {

  ## check if 2D or 3D case is supplied
  if (length(point) == 2) {
    ## ensure the line coords are also 2D
    if (!length(line_coord1) == 2) {
      stop("All coordinates must be either all 2D or all 3D (not a mix of each)"
           )
    }
    if (!length(line_coord2) == 2) {
      stop("All coordinates must be either all 2D or all 3D (not a mix of each)"
      )
    }

    ## Compute
    v1 <- line_coord1 - line_coord2
    v2 <- point - line_coord1
    m <- cbind(v1, v2)
    dist <- abs(det(m)) / sqrt(sum(v1 * v1))
  }

  ## if above is false, it's 3D
  if (length(point) == 3) {
    ## ensure the line coords are also 2D
    if (!length(line_coord1) == 3) {
      stop("All coordinates must be either all 2D or all 3D (not a mix of each)"
           )
    }
    if (!length(line_coord2) == 3) {
      stop("All coordinates must be either all 2D or all 3D (not a mix of each)"
      )
    }

    ## Compute
    v1 <- line_coord1 - line_coord2
    v2 <- point - line_coord1
    v3 <- get_3d_cross_prod(v1, v2)
    area <- sqrt(sum(v3 * v3)) / 2
    dist <- 2 * area / sqrt(sum(v1 * v1))
  }

  ## export
  return(dist)
}


################################ get_3d_cross_prod ##############################

#' Compute the cross product of two 3D vectors
#'
#' @param v1 First vector, as c(x,y,z)
#' @param v2 Second vector, as c(x,y,z)
#'
#' @return A vector of length 3 that describes the cross-product
#'
#' @author Vikram B. Baliga
#'
#' @family mathematical functions
#'
#' @export
#'
#' @examples
#' v1 <- c(1, 1, 3)
#' v2 <- c(3, 1, 3)
#' get_3d_cross_prod(v1, v2)

get_3d_cross_prod <- function(v1,
                             v2){
  v3 <- vector()
  v3[1] <- v1[2] * v2[3] - v1[3] * v2[2]
  v3[2] <- v1[3] * v2[1] - v1[1] * v2[3]
  v3[3] <- v1[1] * v2[2] - v1[2] * v2[1]
  return(v3)
}

#################################### rad_2_deg ###################################

#' Convert radians to degrees
#'
#' @param rad Radians (a numeric of any length >= 1)
#'
#' @return The angle(s) in degrees (as a numeric vector of the same length)
#'
#' @author Vikram B. Baliga
#'
#' @family mathematical functions
#'
#' @export
#'
#' @examples
#' ## One input
#' rad_2_deg(pi/2)
#'
#' ## Multiple inputs
#' rad_2_deg(c(pi / 2, pi, 2 * pi))

rad_2_deg <- function(rad) {

  ## Check that it's a numeric
  #if (!any(class(rad) == "numeric")) {
  #  stop("Input angle must be a numeric")
  #}

  return((rad * 180) / (pi))

}


#################################### deg_2_rad ###################################

#' Convert degrees to radians
#'
#' @param deg Degrees (a numeric of any length >= 1)
#'
#' @return The angle(s) in radians (as a numeric vector of the same length)
#'
#' @author Vikram B. Baliga
#'
#' @family mathematical functions
#'
#' @export
#'
#' @examples
#' ## One input
#' deg_2_rad(90)
#'
#' ## Multiple inputs
#' deg_2_rad(c(5, 10, 15, 20))

deg_2_rad <- function(deg) {

  ## Check that it's a numeric
  #if (!any(class(deg) == "numeric")) {
  #  stop("Input angle must be a numeric")
  #}

  return((deg * pi) / (180))

}


################################## get_2d_angle ###############################

#' Compute an angle in 2D space
#'
#' @param x1 x-coordinate of first point
#' @param y1 y-coordinate of first point
#' @param x2 x-coordinate of second point (vertex)
#' @param y2 y-coordinate of second point (vertex)
#' @param x3 x-coordinate of third point
#' @param y3 y-coordinate of third point
#'
#' @details Everything supplied to arguments must be singular numeric values.
#' The second point (x2, y2) is treated as the vertex, and the angle between
#' the three points in 2D space is computed.
#'
#' @return A numeric vector that provides the angular measurement in degrees.
#'
#' @author Vikram B. Baliga
#'
#' @family mathematical functions
#'
#' @export
#'
#' @examples
#' get_2d_angle(
#'   0, 1,
#'   0, 0,
#'   1, 0)

get_2d_angle <- function(x1, y1,
                         x2, y2,
                         x3, y3) {

  ## re-assignment, to avoid confusion
  i1 <- x1
  i2 <- x2
  i3 <- x3
  j1 <- y1
  j2 <- y2
  j3 <- y3

  ## compute angle
  a <- c(i1, j1) - c(i2, j2)
  b <- c(i3, j3) - c(i2, j2)
  theta <- acos(sum(a * b) / (sqrt(sum(a * a)) * sqrt(sum(b * b)))) * (180 /
                                                                         pi)
  ## export
  return(theta)
}


################################## get_3d_angle ###############################

#' Compute an angle in 3D space
#'
#' @param x1 x-coordinate of first point
#' @param y1 y-coordinate of first point
#' @param z1 z-coordinate of first point
#' @param x2 x-coordinate of second point (vertex)
#' @param y2 y-coordinate of second point (vertex)
#' @param z2 y-coordinate of second point (vertex)
#' @param x3 x-coordinate of third point
#' @param y3 y-coordinate of third point
#' @param z3 z-coordinate of third point
#'
#' @details Everything supplied to arguments must be singular numeric values.
#' The second point (x2, y2, z2) is treated as the vertex, and the angle between
#' the three points in 3D space is computed.
#'
#' @return A numeric vector that provides the angular measurement in degrees.
#'
#' @author Vikram B. Baliga
#'
#' @family mathematical functions
#'
#' @export
#'
#' @examples
#' get_3d_angle(
#'   0, 1, 0,
#'   0, 0, 0,
#'   1, 0, 0)

get_3d_angle <- function(x1, y1, z1,
                         x2, y2, z2,
                         x3, y3, z3) {

  ## compute diffs
  i1 <- x2 - x1
  i2 <- x2 - x3
  j1 <- y2 - y1
  j2 <- y2 - y3
  k1 <- z2 - z1
  k2 <- z2 - z3

  ## compute angle
  dotprod <- (i1 * i2) + (j1 * j2) + (k1 * k2)
  len1 <- sqrt(i1 ^ 2 + j1 ^ 2 + k1 ^ 2)
  len2 <- sqrt(i2 ^ 2 + j2 ^ 2 + k2 ^ 2)
  theta <- acos(dotprod / (len1 * len2)) * (180 / pi)

  ## export
  return(theta)
}


################################# find_curve_elbow #############################

#' Find the "elbow" of a curve.
#'
#' For bivariate data that show monotonic decreases (e.g. plots of trajectory
#' count vs. frame gap allowed, or scree plots from PCAs), this function will
#' find the "elbow" point. This is done by drawing an (imaginary) line between
#' the first observation and the final observation. Then, the distance between
#' that line and each observation is calculated. The "elbow" of the curve is the
#' observation that maximizes this distance.
#'
#' @param data_frame A two-column data frame (numeric entries only)
#' @param export_type If "row_num" (the default), the row number of the elbow
#'   point is returned. If anything else, the entire row of the original data
#'   frame is returned.
#' @param plot_curve Default FALSE; should the curve be plotted?
#'
#' @return If \code{export_type} is \code{row_num} the row number of the elbow
#'   point is returned. If anything else is used for that argument, the entire
#'   row of the original data frame on which the "elbow" is located is returned.
#'   If \code{plot_curve} is \code{TRUE}, the curve is plotted along with a
#'   vertical line drawn at the computed elbow point.
#'
#' @author Vikram B. Baliga
#'
#' @family mathematical functions
#'
#' @export
#'
#' @examples
#' df <- data.frame(x = seq(1:10),
#'                  y = 1/seq(1:10))
#' plot(df)
#' find_curve_elbow(df, plot_curve = TRUE)

find_curve_elbow <- function(data_frame,
                             export_type = "row_num",
                             plot_curve = FALSE) {

  ## Check that there are exactly two columns provided
  if (!dim(data_frame)[2] == 2) {
    stop("The input data has more than two columns.
Please ensure there are only two columns, ordered x-axis first, y-axis second")
  }

  ## Convert to matrix for speedier handling
  data_matrix <- as.matrix(data_frame)
  first_col   <- data_matrix[,1]
  second_col  <- data_matrix[,2]

  ## Get set up for point-line distance computations
  len <- nrow(data_matrix)
  first_point <- c(first_col[1],   second_col[1])
  end_point   <- c(first_col[len], second_col[len])

  ## Compute the distance between each {x, y} and the line defined by the
  ## extreme endpoints.
  ## The "elbow" in the plot will be at the frame gap that maximizes this
  ## distance
  mfg_dists <- NULL
  for (k in 1:len) {
    mfg_dists[k] <-
      get_dist_point_line(
        point = c(first_col[k],
                  second_col[k]),
        line_coord1 = first_point,
        line_coord2 = end_point
      )
  }

  ## Set elbow to the maximum of these distances
  elbow <- which.max(mfg_dists)

  if(plot_curve == TRUE){
    plot(data_matrix); graphics::abline(v = elbow)
  }

  ## Export
  if (export_type == "row_num"){
    return(elbow)
  } else {
    return(data_matrix[elbow,])
  }

}


#########################        calc_vis_angle_V       ########################

#' Estimate visual angles in a V-shaped tunnel
#'
#' Based on animal head positions in a V-shaped tunnel tunnel,
#' \code{calc_vis_angle_V()} calculates the visual angles created by lateral
#' visual stimuli.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"} and
#'   \code{treatments_added}.
#' @param simplify_output If TRUE, the returned object includes the minimum
#'   distance between the subject and the lateral walls as well as the visual
#'   angles (in degrees) to those points on the walls. If FALSE, the returned
#'   object includes all variables internal to the calculation.
#'
#' @details \code{cal_vis_angle_V} assumes the subject's gaze is fixed at the
#'   point on the either side of the tunnel that minimizes the distance to
#'   visual stimuli and therefore maximizes visual angles. All length
#'   measurements are reported in meters.
#'
#' @return A tibble or data.frame with added variables for
#'   \code{height_2_vertex}, \code{height_2_screen}, \code{width_2_screen_pos},
#'   \code{width_2_screen_neg}, \code{min_dist_pos}, \code{min_dist_neg},
#'   \code{bound_pos}, \code{bound_neg}, \code{vis_angle_pos_rad},
#'   \code{vis_angle_neg_rad}, \code{vis_angle_pos_deg},
#'   \code{vis_angle_neg_deg}
#'
#' @author Eric R. Press
#'
#' @family visual perception functions
#'
#' @export

calc_vis_angle_V <- function(obj_name,
                             simplify_output = FALSE){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "treatments_added")){
    stop("Please run insert_treatments() prior to use")
  }

  ## duplicate object for simplify = TRUE
  obj_simplify <- obj_name

  ## Introduce variables for height_2_vertex and height_2_screen
  obj_name$height_2_vertex <-
    abs(obj_name$vertex_height) + obj_name$position_height
  obj_name$height_2_screen <-
    obj_name$height_2_vertex -
    (abs(obj_name$position_width) / tan(obj_name$vertex_angle))


  ## Introduce variables for width_2_screen on positive and negative sides
  ## of the tunnel.
  ## width_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  obj_name$width_2_screen_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$height_2_screen * tan(obj_name$vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(obj_name$vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE

  obj_name$width_2_screen_neg <-
    ifelse(obj_name$position_width < 0, # if in negative side of tunnel
           obj_name$height_2_screen * tan(obj_name$vertex_angle), # TRUE
           (obj_name$height_2_screen * tan(obj_name$vertex_angle)) +
             (2 * abs(obj_name$position_width))) # FALSE

  ## Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen (axis of gaze is orthogonal to plane of each screen)
  obj_name$min_dist_pos <-
    obj_name$width_2_screen_pos * sin((pi/2) - obj_name$vertex_angle)
  # min_dist to positive screen
  obj_name$min_dist_neg <-
    obj_name$width_2_screen_neg * sin((pi/2) - obj_name$vertex_angle)
  # min_dist to negative screen


  ## When the subject is outside the boundaries created by orthogonal planes to
  ## each wall, erroneous visual angles are calculated.
  ## Therefore we must adjust min_dist values according to position_width

  ## Create variable holding the boundary values for each observation
  obj_name$bound_pos <-
    obj_name$height_2_vertex * tan(pi/2 - obj_name$vertex_angle)
  obj_name$bound_neg <-
    obj_name$height_2_vertex * -tan(pi/2 - obj_name$vertex_angle)


  obj_name$min_dist_pos <- # overwrite min_dist_pos
  ifelse(obj_name$position_width <= 0 &
         obj_name$position_width <= obj_name$bound_neg,
         # if position_width is positive and greater than the boundary value
          sqrt(obj_name$height_2_vertex^2 + obj_name$position_width^2),
         # return distance to vertex
          obj_name$min_dist_pos)
         # reurn original min_dist_pos calculation

  obj_name$min_dist_neg <-
  ifelse(obj_name$position_width >= 0 &
         obj_name$position_width >= obj_name$bound_pos,
         # if position_width is negative and smaller than the boundary value
          sqrt(obj_name$height_2_vertex^2 + obj_name$position_width^2),
         # return distance to vertex
          obj_name$min_dist_neg)
         # return original min_dist_neg calculation


  ## Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add these variables into the dataframe.
  obj_name$vis_angle_pos_rad <-
    2*atan(obj_name$stim_param_pos/(2*obj_name$min_dist_pos)) # radians
  obj_name$vis_angle_neg_rad <-
    2*atan(obj_name$stim_param_neg/(2*obj_name$min_dist_neg)) # radians

  obj_name$vis_angle_pos_deg <- rad_2_deg(obj_name$vis_angle_pos_rad) # degrees
  obj_name$vis_angle_neg_deg <- rad_2_deg(obj_name$vis_angle_neg_rad) # degrees

  ## simplify = TRUE output
  obj_simplify$min_dist_pos <- obj_name$min_dist_pos
  obj_simplify$min_dist_neg <- obj_name$min_dist_neg
  obj_simplify$vis_angle_pos_deg <- rad_2_deg(obj_name$vis_angle_pos_rad)
  obj_simplify$vis_angle_neg_deg <- rad_2_deg(obj_name$vis_angle_neg_rad)

  ## return simple or complete data table based on simplify argument
  if(simplify_output == TRUE){
    return(obj_simplify)
  } else {
    return(obj_name)
    }
}


#########################        calc_vis_angle_box       ##########################

#' Estimate visual angles in a box-shaped tunnel
#'
#' Based on  animal head positions in a rectangular tunnel i.e. box,
#' \code{calc_vis_angle_box()} calculates the visual angles created by lateral
#' visual stimuli.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"} and
#'   \code{treatments_added}.
#'
#' @details \code{cal_vis_angle_box} assumes fixed gaze at the point on the
#'   either side of the tunnel that minimizes the distance to visual stimuli and
#'   thereby maximizes visual angles. All length measurements reported in
#'   meters.
#'
#' @return A tibble or data.frame with added variables for \code{min_dist_pos},
#'   \code{min_dist_neg}, \code{vis_angle_pos_rad}, \code{vis_angle_neg_rad},
#'   \code{vis_angle_pos_deg}, and \code{vis_angle_neg_deg}.
#'
#' @author Eric R. Press
#'
#' @family visual perception functions
#'
#' @export


calc_vis_angle_box <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments() has been run
  if (!any(attr(obj_name,"pathviewR_steps") == "treatments_added")){
    stop("Please run insert_treatments() prior to use")
  }

  ## Calculate minimum distance to each screen
  obj_name$min_dist_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$pos_wall - obj_name$position_width, # TRUE
           obj_name$pos_wall + abs(obj_name$position_width) # FALSE
           )

  obj_name$min_dist_neg <-
    ifelse(obj_name$position_width <= 0, # if in negative side of tunnel
           abs(obj_name$neg_wall) - abs(obj_name$position_width), # TRUE
           abs(obj_name$neg_wall - obj_name$position_width) # FALSE
           )

  ## Calculate visual angles (radians and degrees) using distance to
  ## positive and negative screens. Add these variables into the dataframe.
  obj_name$vis_angle_pos_rad <-
    2*atan(obj_name$stim_param_pos / (2*obj_name$min_dist_pos)) # radians
  obj_name$vis_angle_neg_rad <-
    2*atan(obj_name$stim_param_neg / (2*obj_name$min_dist_neg)) # radians

  obj_name$vis_angle_pos_deg <-
    rad_2_deg(as.numeric(obj_name$vis_angle_pos_rad)) # degrees
  obj_name$vis_angle_neg_deg <-
    rad_2_deg(as.numeric(obj_name$vis_angle_neg_rad)) # degrees

  return(obj_name)
}


###############################    calc_sf_V     ###############################

#' Estimate spatial frequency of visual stimuli in V-shaped tunnel
#'
#' Based on animal head positions in a V-shaped tunnel,
#' \code{calc_sf_V()} calculates how the animal perceives the spatial frequency
#' of lateral visual stimuli as modulated by distance to the stimulus.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#' \code{pathviewR_steps} that includes \code{"viewr"}
#' @param vertex_angle The angle (in degrees) subtended by a vertical axis and
#' the sides of the tunnel. Equivalent to the angle of the "V" divided by 2.
#' \code{vertex_angle)} defaults to 45.
#' @param stim_param_pos The length (in meters) of the visual stimulus presented
#' on the positive side of the tunnel (i.e. \code{position_width >= 0}). For
#' example, a sinusoidal grating 10cm wide is \code{stim_param_pos = 0.1}
#' @param stim_param_neg The same convention as \code{stim_param_pos} but for
#' stimuli presented on the negative side of the tunnel
#' (i.e. \code{position_width < 0}).
#' @param simplify_output If TRUE, the returned object includes minimum distance
#' as well as the spatial frequencies perceived on either side of the
#' tunnel. If FALSE, the returned object includes all variables internal to the
#' calculation.
#'
#' @details \code{cal_sf_V} assumes fixed gaze at the point on the
#' either side of the tunnel that minimizes the distance to visual stimuli and
#' thereby minimizes spatial frequencies.
#'
#' @return A tibble or data.frame with added variables for
#' \code{s_freq_pos} and \code{s_freq_neg} reported in cycles per 1deg of visual
#' angle.
#'
#' @author Eric R. Press
#'
#' @family visual perception functions
#'
#' @export


calc_sf_V <- function(obj_name,
                      vertex_angle,
                      stim_param_pos,
                      stim_param_neg,
                      simplify_output = FALSE) {

  ## Check that it's a viewr object
  if (!any(attr(obj_name, "pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that get_full_trajectories has been run
  if (!any(attr(obj_name, "pathviewR_steps") == "treatments_added")) {
    stop("Run insert_treatments() prior to use.")
  }

  ## duplicate object for simplify = TRUE
  obj_simplify <- obj_name

  ## Introduce variables for height_2_vertex and height_2_screen
  obj_name$height_2_vertex <-
    abs(obj_name$vertex_height) + obj_name$position_height
  obj_name$height_2_screen <- obj_name$height_2_vertex -
    (abs(obj_name$position_width) / tan(obj_name$vertex_angle))


  ## Introduce variables for width_2_screen on positive and negative sides
  ## of the tunnel.
  ## width_2_screen refers to the horizontal distance between the bird and
  ## either screen.
  obj_name$width_2_screen_pos <-
    ifelse(
      obj_name$position_width >= 0,
      # if in positive side of tunnel
      obj_name$height_2_screen * tan(obj_name$vertex_angle),
      # TRUE
      (obj_name$height_2_screen * tan(obj_name$vertex_angle)) +
        (2 * abs(obj_name$position_width))
    ) # FALSE

  obj_name$width_2_screen_neg <-
    ifelse(
      obj_name$position_width < 0,
      # if in negative side of tunnel
      obj_name$height_2_screen * tan(obj_name$vertex_angle),
      # TRUE
      (obj_name$height_2_screen * tan(obj_name$vertex_angle)) +
        (2 * abs(obj_name$position_width))
    ) # FALSE


  ## When the bird is outside the boundaries created by orthogonal planes to
  ## each screen, erroneous visual angles are calculated based on a
  ## minimum distance to either screen.
  ## Therefore min_dist values need to be adjusted according to position_width

  ## Create variable of boundary values for each observation
  obj_name$bound_pos <-
    obj_name$height_2_vertex * tan(deg_2_rad(90) - obj_name$vertex_angle)
  obj_name$bound_neg <-
    obj_name$height_2_vertex * -tan(deg_2_rad(90) - obj_name$vertex_angle)


  ## Introduce variable min_dist on positive and negative sides of the
  ## tunnel. min_dist refers to the minimum distance between the bird and either
  ## screen.
  obj_name$min_dist_pos <-
    ifelse(
      obj_name$position_width <= 0 &
        obj_name$position_width <= obj_name$bound_neg,
      # if position_width is negative and less than the boundary value
      sqrt(obj_name$height_2_vertex^2 + obj_name$position_width^2),
      # return distance to vertex
      obj_name$width_2_screen_pos * sin(deg_2_rad(90) - obj_name$vertex_angle))
      # return minimum distance to positive screen

  obj_name$min_dist_neg <-
    ifelse(
      obj_name$position_width >= 0 &
        obj_name$position_width >= obj_name$bound_pos,
      # if position_width is positive and greater than the boundary value
      sqrt(obj_name$height_2_vertex^2 + obj_name$position_width ^2),
      # return distance to vertex
      obj_name$width_2_screen_neg * sin(deg_2_rad(90) - obj_name$vertex_angle))
      # return minimum distance to negative screen


  ## Calculate distance along plane of screen equal to 1deg of visual angle.
  deg_dist_pos <- 2 * obj_name$min_dist_pos * tan(deg_2_rad(1))
  deg_dist_neg <- 2 * obj_name$min_dist_neg * tan(deg_2_rad(1))

  ## Calculate spatial frequency as number of cycles of stimulus per 1deg of
  ## visual angle.
  obj_name$sf_pos <- deg_dist_pos / obj_name$stim_param_pos
  obj_name$sf_neg <- deg_dist_neg / obj_name$stim_param_neg


  ## Create simple data frame by adding min_dist and spatial frequencies
  obj_simplify$min_dist_pos <- obj_name$min_dist_pos
  obj_simplify$min_dist_neg <- obj_name$min_dist_neg
  obj_simplify$sf_pos <- obj_name$sf_pos
  obj_simplify$sf_neg <- obj_name$sf_neg

  ## Leave note that spatial frequencies were calculated on dataset
  attr(obj_name, "pathviewR_steps") <-
    c(attr(obj_name, "pathviewR_steps"),
      "sf_calculated")

  ## return simple or complete data table based on simplify argument
  if (simplify_output == TRUE) {
    return(obj_simplify)
  } else {
    return(obj_name)
  }
}



##################################  calc_sf_box   ##############################

#' Estimate spatial frequency of visual stimuli in rectangular tunnel, i.e. box
#'
#' Based on animal head positions in a rectangular tunnel i.e. box,
#' \code{calc_sf_box()} calculates how the animal perceives the spatial
#' frequency of lateral visual stimuli as modulated by distance to the stimulus.
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#'
#' @details \code{calc_sf_box} assumes fixed gaze at the point on the
#' either side of the tunnel that minimizes the distance to visual stimuli and
#' thereby minimizes spatial frequencies.
#'
#' @return A tibble or data.frame with added variables \code{min_dist_pos} and
#'   \code{min_dist_neg} describing the minimum distance to the tunnel walls as
#'   well as \code{s_freq_pos} and \code{s_freq_neg} reported in cycles per 1deg
#'   of visual angle.
#'
#' @author Eric R. Press
#'
#' @family visual perception functions
#'
#' @export


calc_sf_box <- function(obj_name){

  ## Check that it's a viewr object
  if (!any(attr(obj_name, "pathviewR_steps") == "viewr")){
    stop("This doesn't seem to be a viewr object")
  }

  ## Check that insert_treatments has been run
  if (!any(attr(obj_name, "pathviewR_steps") == "treatments_added")){
    stop("Please run insert_treatments() prior to use")
  }

  ## Calculate minimum distance to each screen
  obj_name$min_dist_pos <-
    ifelse(obj_name$position_width >= 0, # if in positive side of tunnel
           obj_name$pos_wall - obj_name$position_width, # TRUE
           obj_name$pos_wall + abs(obj_name$position_width) # FALSE
          )

  obj_name$min_dist_neg <-
    ifelse(obj_name$position_width <= 0, # if in negative side of tunnel
           abs(obj_name$neg_wall) - abs(obj_name$position_width), # TRUE
           abs(obj_name$neg_wall - obj_name$position_width) # FALSE
          )

  ## Calculate distance along plane of the wall equal to 1deg of visual angle.
  deg_dist_pos <- 2 * obj_name$min_dist_pos * tan(deg_2_rad(1))
  deg_dist_neg <- 2 * obj_name$min_dist_neg * tan(deg_2_rad(1))

  ## Calculate spatial frequency as number of cycles of stimulus per 1deg of
  ## visual angle.
  obj_name$s_freq_pos <- deg_dist_pos / obj_name$stim_param_pos
  obj_name$s_freq_neg <- deg_dist_neg / obj_name$stim_param_neg

  ## Leave note that spatial frequencies were calculated on dataset
  attr(obj_name, "pathviewR_steps") <- c(attr(obj_name, "pathviewR_steps"),
                                         "frequencies_calculated")

  return(obj_name)
}

