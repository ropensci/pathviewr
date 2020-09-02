## Part of the pathviewR package
## Last updated: 2020-09-02 VBB


########################### visualize_frame_gap_choice #########################

#' Visualize the consequence of using various max_frame_gap values
#'
#' Run separate_trajectories() with many different frame gaps to help determine
#' what value to use
#'
#' @param obj_name The input viewr object; a tibble or data.frame with attribute
#'   \code{pathviewR_steps} that includes \code{"viewr"}
#' @param loops How many total frame gap entries to consider
#' @param ... Additional arguments
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
#' @export

visualize_frame_gap_choice <- function(obj_name,
                                       loops = 20,
                                       ...){

  ## Check that it's a viewr object
  if (!any(attr(obj_name,"pathviewR_steps") == "viewr")) {
    stop("This doesn't seem to be a viewr object")
  }

  # make a bunch of empty vectors to dump info
  mfg <- vector("list", loops)
  cts <- vector("list", loops)
  trajectory_count <- vector(mode = "double", loops)
  frame_gap_allowed <- vector(mode = "double", loops)

  # loop through user defined number of max frame gap values
  i <- 1
  while (i < loops + 1) {
    mfg[[i]] = quick_separate_trajectories(obj_name, max_frame_gap = i)
    cts[[i]] = count(mfg[[i]], traj_id)
    trajectory_count[i] = nrow(cts[[i]])
    frame_gap_allowed[i] = i
    i = i +1
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
#'   \code{pathviewR_steps} that includes \code{"viewr"}) that has been passed
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
#' @author Vikram B. Baliga
#'
#' @family plotting functions

plot_viewr_trajectories <- function(obj_name,
                                    plot_axes = c("length", "width"),
                                    multi_plot = FALSE){

  ## Collect names of trajectories
  obj_name_trajs <- unique(obj_name$file_sub_traj)
  ## For multi-plotting, find out what the square root of the total number
  ## of trajectories is. This will be used to set par(mfrow()).
  sqrt_traj_count <- ceiling(sqrt(length(obj_name_trajs)))

  if (plot_axes[1] == "length"){
    if (plot_axes[2] == "width") {
      if (multi_plot == TRUE) {
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
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
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
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
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
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
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
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
      if (multi_plot == TRUE) {
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
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
        par(mar = c(0, 0, 0, 0))
        par(mfrow = c(sqrt_traj_count, sqrt_traj_count))
      }
      for (i in 1:length(obj_name_trajs)){
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

}


############################### plot by bird ###############################
## Generate plots of each individual--hoping to loop to auto go through all birds in each treatment...

purrplot_by_bird <- function(obj_name,
                             treatment,
                             ...){
  #set axes limits based on data
  height_limits <- c(max(abs(range(obj_name$position_height)))*-1,
                     max(abs(range(obj_name$position_height))))
  width_limits <- c(max(abs(range(obj_name$position_width)))*-1,
                    max(abs(range(obj_name$position_width))))

  #for top view (change in lateral position)
  top_view <- obj_name %>%
    group_by(subject) %>%
    nest() %>%
    mutate(paths = map(data, ~ggplot(., aes(position_length, position_width, colour = treatment)) +
                         geom_point(alpha = .1, show.legend = FALSE) +
                         ylim(width_limits) +
                         geom_hline(yintercept = 0, linetype = "dotted") +
                         coord_fixed(ratio = 1) +
                         theme_tufte()),
           hist = map(data, ~ggplot(., aes(x = position_width, fill = treatment)) +
                        geom_density(alpha = .5, position = "identity", show.legend = FALSE) +
                        xlim(width_limits) +
                        geom_vline(xintercept = 0, linetype = "dotted") +
                        coord_flip() +
                        theme_tufte()),
           filename = paste0(subject,"_top",".png"))

  #all of them together:
  top_all_plots <- top_view %>%
    select(subject, paths, hist) %>%
    gather("plot_type", "allplots", 2:3)

  birdseye_view <- plot_grid(plotlist = top_all_plots[[3]])

  ggsave(paste0(treatment,"_", "topview.png"), birdseye_view,
         path = "C:/Users/Melis/Documents/GoogleDrive/Altshuler/thesis/ZFVG/R plots/bybird")

  #for elev view (change in height):
  elev_view <- obj_name %>%
    group_by(subject) %>%
    nest() %>%
    mutate(paths = map(data, ~ggplot(., aes(position_length, position_height, colour = treatment)) +
                         geom_point(alpha = .1, show.legend = FALSE) +
                         ylim(height_limits) +
                         geom_hline(yintercept = 0, linetype = "dotted") +
                         coord_fixed(ratio = 1) +
                         theme_tufte()),
           hist = map(data, ~ggplot(., aes(position_height, fill = treatment)) +
                        geom_density(alpha = .5, position = "identity", show.legend = FALSE) +
                        xlim(height_limits) +
                        geom_vline(xintercept = 0, linetype = "dotted") +
                        coord_flip() +
                        theme_tufte()),
           filename = paste0(subject,"_elev",".png"))

  #all of them together:
  elev_all_plots <- elev_view %>%
    select(subject, paths, hist) %>%
    gather("plot_type", "allplots", 2:3)

  side_view <- plot_grid(plotlist = elev_all_plots[[3]])

  ggsave(paste0(treatment,"_", "elevview.png"), side_view,
         path = "C:/Users/Melis/Documents/GoogleDrive/Altshuler/thesis/ZFVG/R plots/bybird")
}

#to save each plot separately:
#AB_top %>%
#  select(filename, paths) %>%
#  pwalk(ggsave, path = "C:/Users/Melis/Documents/GoogleDrive/Altshuler/thesis/ZFVG/R plots/bybird")
