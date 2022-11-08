## Part of the pathviewr package
## Last updated: 2020-09-16 VBB & MSA


########################### visualize_frame_gap_choice #########################

#' Visualize the consequence of using various max_frame_gap values
#'
#' Run separate_trajectories() with many different frame gaps to help determine
#' what value to use
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewr_steps} that includes \code{"viewr"}
#' @param loops How many total frame gap entries to consider. Each loop will
#'   increase the \code{max_fram_gap} argument in \code{separate_trajectories}
#'   by 1.
#' @param ... Additional arguments
#'
#' @details The input viewr object (\code{obj_name}) should likely be an object
#'   that has passed through the \code{select_x_percent()} step.
#'
#' @return A plot and a tibble, each of which shows the total number of
#'   trajectories that result from using the specified range of
#'   \code{max_frame_gap} values.
#'
#' @author Melissa S. Armstrong and Vikram B. Baliga
#'
#' @family data cleaning functions
#' @family plotting functions
#' @family functions that define or clean trajectories
#'
#' @importFrom graphics plot
#'
#' @export
#'
#' @examples
#' library(pathviewr)
#'
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
#'                              package = 'pathviewr'))
#'
#' motive_selected <-
#'   motive_data %>%
#'   relabel_viewr_axes() %>%
#'   gather_tunnel_data() %>%
#'   trim_tunnel_outliers() %>%
#'   rotate_tunnel() %>%
#'   get_velocity() %>%
#'   select_x_percent(desired_percent = 50)
#'
#' visualize_frame_gap_choice(motive_selected, loops = 10)

visualize_frame_gap_choice <- function(obj_name,
                                       loops = 20,
                                       ...){

  ## Check that it's a viewr object
  # if (!any(attr(obj_name,"pathviewr_steps") == "viewr")) {
  #   stop("This doesn't seem to be a viewr object")
  # }

  # make a bunch of empty vectors to dump info
  mfg <- vector("list", loops)
  cts <- vector("list", loops)
  trajectory_count <- vector(mode = "double", loops)
  frame_gap_allowed <- vector(mode = "double", loops)

  # loop through user defined number of max frame gap values
  i <- 1
  while (i < loops + 1) {
    mfg[[i]] <- quick_separate_trajectories(obj_name, max_frame_gap = i)
    cts[[i]] <- dplyr::count(mfg[[i]], traj_id)
    trajectory_count[i] <- nrow(cts[[i]])
    frame_gap_allowed[i] <- i
    i <- i +1
  }

  ## Collect the info on max frame gaps allowed vs. trajectory counts
  mfg_tib <- tibble::tibble(frame_gap_allowed,
                            trajectory_count)

  ## Find the curve elbow point via `find_curve_elbow()`
  max_fg <- find_curve_elbow(mfg_tib,
                             export_type = "row_num",
                             plot_curve = FALSE)

  ## Paste filename into export tibble
  obj_name_arg <- deparse(substitute(obj_name))
  mfg_tib$file_id <- as.character(obj_name_arg)

  mfg_plot <- plot(mfg_tib$frame_gap_allowed,
                   mfg_tib$trajectory_count); graphics::abline(v = max_fg)

  return(list(mfg_tib, mfg_plot))

}


########################### plot_viewr_trajectories ############################

#' Plot each trajectory within a viewr object
#'
#' @param obj_name A viewr object (a tibble or data.frame with attribute
#'   \code{pathviewr_steps} that includes \code{"viewr"}) that has been passed
#'   through \code{separate_trajectories()} or \code{get_full_trajectories()}.
#' @param plot_axes Which position axes should be plotted? A character vector
#'   including exactly two of the following choices must be supplied:
#'   \code{length}, \code{width}, \code{height}. Default is c("length",
#'   "width").
#' @param multi_plot Should separate plots (one per trajectory) be created or
#'   should one multi-plot grid be generated. Defaults to FALSE, which produces
#'   separate plots.
#'
#' @return A (base-R) series of plots or single plot (if \code{multi_plot =
#'   TRUE}) that depict each trajectory along the chosen axes.
#'
#' @export
#'
#' @importFrom graphics plot
#'
#' @author Vikram B. Baliga
#'
#' @family plotting functions
#'
#' @examples
#' library(pathviewr)
#'
#' ## Import the example Motive data included in the package
#' motive_data <-
#'   read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
#'                              package = 'pathviewr'))
#'
#' motive_full <-
#'   motive_data %>%
#'   clean_viewr(desired_percent = 50,
#'               max_frame_gap = "autodetect",
#'               span = 0.95)
#'
#' plot_viewr_trajectories(motive_full, multi_plot = FALSE)
#' plot_viewr_trajectories(motive_full, multi_plot = TRUE)

plot_viewr_trajectories <- function(obj_name,
                                    plot_axes = c("length", "width"),
                                    multi_plot = FALSE){

  ## Collect names of trajectories
  obj_name_trajs <- unique(obj_name$file_sub_traj)
  ## For multi-plotting, find out what the square root of the total number
  ## of trajectories is. This will be used to set par(mfrow()).
  sqrt_traj_count <- ceiling(sqrt(length(obj_name_trajs)))

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))

  if (plot_axes[1] == "length"){
    if (plot_axes[2] == "width") {
      if (multi_plot == TRUE) {
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in seq_len(length(obj_name_trajs))){
        tmp <- obj_name %>% dplyr::filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_length,
             tmp$position_width,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
             ## add sub-title to indicate i
             sub = paste0("trajectory #",i),
             ## keep the same dimensions across all plots:
             xlim = c(min(obj_name$position_length),
                      max(obj_name$position_length)),
             ylim = c(min(obj_name$position_width),
                      max(obj_name$position_width))
        )
      }
    }
    if (plot_axes[2] == "height") {
      if (multi_plot == TRUE) {
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in seq_len(length(obj_name_trajs))){
        tmp <- obj_name %>% dplyr::filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_length,
             tmp$position_height,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
             ## add sub-title to indicate i
             sub = paste0("trajectory #",i),
             ## keep the same dimensions across all plots:
             xlim = c(min(obj_name$position_length),
                      max(obj_name$position_length)),
             ylim = c(min(obj_name$position_height),
                      max(obj_name$position_height))
        )
      }
    }
  }

  if (plot_axes[1] == "width"){
    if (plot_axes[2] == "length") {
      if (multi_plot == TRUE) {
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in seq_len(length(obj_name_trajs))){
        tmp <- obj_name %>% dplyr::filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_width,
             tmp$position_length,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
             ## add sub-title to indicate i
             sub = paste0("trajectory #",i),
             ## keep the same dimensions across all plots:
             xlim = c(min(obj_name$position_width),
                      max(obj_name$position_width)),
             ylim = c(min(obj_name$position_length),
                      max(obj_name$position_length))
        )
      }
    }
    if (plot_axes[2] == "height") {
      if (multi_plot == TRUE) {
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in seq_len(length(obj_name_trajs))){
        tmp <- obj_name %>% dplyr::filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_width,
             tmp$position_height,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
             ## add sub-title to indicate i
             sub = paste0("trajectory #",i),
             ## keep the same dimensions across all plots:
             xlim = c(min(obj_name$position_width),
                      max(obj_name$position_width)),
             ylim = c(min(obj_name$position_height),
                      max(obj_name$position_height))
        )
      }
    }
  }

  if (plot_axes[1] == "height"){
    if (plot_axes[2] == "length") {
      oldpar <- par(no.readonly = TRUE)
      on.exit(par(oldpar))
      if (multi_plot == TRUE) {
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in seq_len(length(obj_name_trajs))){
        tmp <- obj_name %>% dplyr::filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_height,
             tmp$position_length,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
             ## add sub-title to indicate i
             sub = paste0("trajectory #",i),
             ## keep the same dimensions across all plots:
             xlim = c(min(obj_name$position_height),
                      max(obj_name$position_height)),
             ylim = c(min(obj_name$position_length),
                      max(obj_name$position_length))
        )
      }
    }
    if (plot_axes[2] == "width") {
      if (multi_plot == TRUE) {
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in seq_len(length(obj_name_trajs))){
        tmp <- obj_name %>% dplyr::filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_height,
             tmp$position_width,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
             ## add sub-title to indicate i
             sub = paste0("trajectory #",i),
             ## keep the same dimensions across all plots:
             xlim = c(min(obj_name$position_height),
                      max(obj_name$position_height)),
             ylim = c(min(obj_name$position_width),
                      max(obj_name$position_width))
        )
      }
    }
  }

  ## reset back to default
  on.exit(par(oldpar))

}

####################### plot_by_subject ##########################

#' Plot trajectories and density plots of position by subject
#'
#' Plots all trajectories and generates density plots of position by subject
#' from elevation and bird's eye views.
#'
#' @param obj_name A viewr object (a tibble or data.frame with attribute
#'   \code{pathviewr_steps} that includes \code{"viewr"}) that has been passed
#'   through \code{separate_trajectories()} or \code{get_full_trajectories()}.
#' @param col_by_treat If multiple treatments or sessions, color data per
#'   treatment or session. Treatments must be levels in a column named
#'   \code{treatment}.
#' @param ... Additional arguments passed to/from other pathviewr functions.
#'
#' @details  The input viewr object should have passed through
#'   \code{separate_trajectories()} or \code{get_full_trajectories()}.
#'   Optionally, treatments should have been added as levels in a column named
#'   \code{treatment}. Two plots will be produced, one from a "bird's eye view"
#'   of width against length and one from an "elevation view" of height against
#'   length. All trajectories will be plotted on a per subject basis, along with
#'   density plots of width or height depending on the view.
#'   \code{col_by_treat = TRUE}, data will be plotted by color according to
#'   treatment in both the trajectory plots and the density plots.
#'
#' @return A "bird's eye view" plot and an "elevation view" plot, made via
#'   ggplot2.
#'
#' @importFrom graphics plot
#'
#' @export
#' @author Melissa S. Armstrong
#' @family plotting functions
#'
#' @examples
#' library(pathviewr)
#' library(ggplot2)
#' library(magrittr)
#'
#' if (interactive()) {
#'   ## Import the example Motive data included in the package
#'   motive_data <-
#'     read_motive_csv(system.file("extdata",
#'                                 "pathviewr_motive_example_data.csv",
#'                                 package = 'pathviewr'))
#'
#'   ## Clean, isolate, and label trajectories
#'   motive_full <-
#'     motive_data %>%
#'     clean_viewr(desired_percent = 50,
#'                 max_frame_gap = "autodetect",
#'                 span = 0.95)
#'
#'   ## Plot all trajectories by subject
#'   motive_full %>%
#'     plot_by_subject()
#'
#'   ## Add treatment information
#'   motive_full$treatment <- c(rep("latA", 100), rep("latB", 100),
#'                              rep("latA", 100), rep("latB", 149))
#'
#'   ## Plot all trajectories by subject, color by treatment
#'   motive_full %>%
#'     plot_by_subject(col_by_treat = TRUE)
#' }


plot_by_subject <- function(obj_name,
                            col_by_treat = FALSE,
                            ...) {

  ## Check that it's a viewr object
  # if (!any(attr(obj_name, "pathviewr_steps") == "viewr")) {
  #   stop("This doesn't seem to be a viewr object")
  # }

  #set axes limits based on data
  height_limits <- c(max(abs(range(
    obj_name$position_height
  ))) * -1,
  max(abs(range(
    obj_name$position_height
  ))))
  width_limits <- c(max(abs(range(
    obj_name$position_width
  ))) * -1,
  max(abs(range(
    obj_name$position_width
  ))))

  if (col_by_treat == FALSE) {
    #for top view
    top_view <- obj_name %>%
      dplyr::group_by(subject) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        paths = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(
            position_length,
            position_width
          )) +
            ggplot2::geom_point(alpha = .1, show.legend = FALSE) +
            ggplot2::ylim(width_limits) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
            ggplot2::coord_fixed(ratio = 1)
        ),
        hist = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(x = position_width)) +
            ggplot2::geom_density(
              alpha = .5,
              position = "identity",
              show.legend = FALSE
            ) +
            ggplot2::xlim(width_limits) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
            ggplot2::coord_flip()
        )
      )

    #all of them together:
    top_all_plots <- top_view %>%
      dplyr::select(subject, paths, hist) %>%
      tidyr::gather("plot_type", "allplots", 2:3)

    p1 <- NULL
    cowA <- cowplot::plot_grid(p1, labels = "")
    cowB <- cowplot::plot_grid(
      plotlist = top_all_plots[[3]],
      labels = top_all_plots$subject,
      label_size = 8,
      label_y = 1.1
    )

    birdseye_view <-
      cowplot::plot_grid(cowA, cowB, ncol = 1, rel_heights = c(0.05, 1))

    #for elev view
    elev_view <- obj_name %>%
      dplyr::group_by(subject) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        paths = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(
            position_length, position_height
          )) +
            ggplot2::geom_point(alpha = .1, show.legend = FALSE) +
            ggplot2::ylim(height_limits) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
            ggplot2::coord_fixed(ratio = 1)
        ),
        hist = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(position_height)) +
            ggplot2::geom_density(
              alpha = .5,
              position = "identity",
              show.legend = FALSE
            ) +
            ggplot2::xlim(height_limits) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
            ggplot2::coord_flip()
        )
      )

    #all of them together:
    elev_all_plots <- elev_view %>%
      dplyr::select(subject, paths, hist) %>%
      tidyr::gather("plot_type", "allplots", 2:3)

    cowC <- cowplot::plot_grid(
      plotlist = elev_all_plots[[3]],
      labels = elev_all_plots$subject,
      label_size = 8,
      label_y = 1.1
    )

    side_view <-
      cowplot::plot_grid(cowA, cowC, ncol = 1, rel_heights = c(0.05, 1))

    return(list(birdseye_view, side_view))
  }

  if (col_by_treat == TRUE) {
    #for top view (change in lateral position)
    top_view <- obj_name %>%
      dplyr::group_by(subject) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        paths = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(
            position_length,
            position_width,
            colour = treatment
          )) +
            ggplot2::geom_point(alpha = .1, show.legend = FALSE) +
            ggplot2::ylim(width_limits) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
            ggplot2::coord_fixed(ratio = 1)
        ),
        hist = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(x = position_width,
                                   fill = treatment)) +
            ggplot2::geom_density(
              alpha = .5,
              position = "identity",
              show.legend = FALSE
            ) +
            ggplot2::xlim(width_limits) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
            ggplot2::coord_flip()
        )
      )

    #all of them together:
    top_all_plots <- top_view %>%
      dplyr::select(subject, paths, hist) %>%
      tidyr::gather("plot_type", "allplots", 2:3)

    p1 <- NULL
    cowA <- cowplot::plot_grid(p1, labels = "")
    cowB <- cowplot::plot_grid(
      plotlist = top_all_plots[[3]],
      labels = top_all_plots$subject,
      label_size = 8,
      label_y = 1.1
    )

    birdseye_view <-
      cowplot::plot_grid(cowA, cowB, ncol = 1, rel_heights = c(0.05, 1))

    #for elev view (change in height):
    elev_view <- obj_name %>%
      dplyr::group_by(subject) %>%
      tidyr::nest() %>%
      dplyr::mutate(
        paths = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(
            position_length, position_height, colour = treatment
          )) +
            ggplot2::geom_point(alpha = .1, show.legend = FALSE) +
            ggplot2::ylim(height_limits) +
            ggplot2::geom_hline(yintercept = 0, linetype = "dotted") +
            ggplot2::coord_fixed(ratio = 1)
        ),
        hist = purrr::map(
          data,
          ~ ggplot2::ggplot(., aes(position_height, fill = treatment)) +
            ggplot2::geom_density(
              alpha = .5,
              position = "identity",
              show.legend = FALSE
            ) +
            ggplot2::xlim(height_limits) +
            ggplot2::geom_vline(xintercept = 0, linetype = "dotted") +
            ggplot2::coord_flip()
        )
      )

    #all of them together:
    elev_all_plots <- elev_view %>%
      dplyr::select(subject, paths, hist) %>%
      tidyr::gather("plot_type", "allplots", 2:3)

    cowC <- cowplot::plot_grid(
      plotlist = elev_all_plots[[3]],
      labels = elev_all_plots$subject,
      label_size = 8,
      label_y = 1.1
    )

    side_view <-
      cowplot::plot_grid(cowA, cowC, ncol = 1, rel_heights = c(0.05, 1))

    return(list(birdseye_view, side_view))
  }

}
