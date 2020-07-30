## Part of the pathviewR package
## Last updated: 2020-07-30 VBB

########################### plot_viewr_trajectories ############################

#' Plot each trajectory within a viewr object
#'
#' @param obj_name A viewr object that has been passed through
#'   \code{separate_trajectories()} or \code{get_full_trajectories()}.
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
#' @author Vikram B. Baliga
#'
#' @family plotting functions

plot_viewr_trajectories <- function(obj_name,
                                    plot_axes = c("length", "width"),
                                    multi_plot = FALSE){

  ## ADD CHECKS LATER AND/OR PROVIDE MEANINGFUL ERROR MESSAGES

  ## Collect names of trajectories
  obj_name_trajs <- unique(obj_name$file_sub_traj)
  ## For multi-plotting, find out what the square root of the total number
  ## of trajectories is and add 1
  sqrt_traj_count <- ceiling(sqrt(length(obj_name_trajs)))

  if (plot_axes[1] == "length"){
    if (plot_axes[2] == "width") {
      if (multi_plot == TRUE) {
        par(mar = c(1, 1, 1, 1))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
        tmp <- obj_name %>% filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_length,
             tmp$position_width,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
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
        par(mar = c(1, 1, 1, 1))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
        tmp <- obj_name %>% filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_length,
             tmp$position_height,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
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
        par(mar = c(1, 1, 1, 1))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
        tmp <- obj_name %>% filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_width,
             tmp$position_length,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
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
        par(mar = c(1, 1, 1, 1))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
        tmp <- obj_name %>% filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_width,
             tmp$position_height,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
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
      if (multi_plot == TRUE) {
        par(mar = c(1, 1, 1, 1))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
        tmp <- obj_name %>% filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_height,
             tmp$position_length,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
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
        par(mar = c(1, 1, 1, 1))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
        tmp <- obj_name %>% filter(file_sub_traj == obj_name_trajs[i])
        plot(tmp$position_height,
             tmp$position_width,
             asp = 1,
             ## add a title that indicates sub_traj
             main = obj_name_trajs[i],
             ## keep the same dimensions across all plots:
             xlim = c(min(obj_name$position_height),
                      max(obj_name$position_height)),
             ylim = c(min(obj_name$position_width),
                      max(obj_name$position_width))
        )
      }
    }
  }

}
