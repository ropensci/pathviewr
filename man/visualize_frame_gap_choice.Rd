% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{visualize_frame_gap_choice}
\alias{visualize_frame_gap_choice}
\title{Visualize the consequence of using various max_frame_gap values}
\usage{
visualize_frame_gap_choice(obj_name, loops = 20, ...)
}
\arguments{
\item{obj_name}{The input viewr object; a tibble or data.frame with attribute
\code{pathviewr_steps} that includes \code{"viewr"}}

\item{loops}{How many total frame gap entries to consider. Each loop will
increase the \code{max_fram_gap} argument in \code{separate_trajectories}
by 1.}

\item{...}{Additional arguments}
}
\value{
A plot and a tibble, each of which shows the total number of
trajectories that result from using the specified range of
\code{max_frame_gap} values.
}
\description{
Run separate_trajectories() with many different frame gaps to help determine
what value to use
}
\details{
The input viewr object (\code{obj_name}) should likely be an object
that has passed through the \code{select_x_percent()} step.
}
\examples{
library(pathviewr)

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                             package = 'pathviewr'))

motive_selected <-
  motive_data \%>\%
  relabel_viewr_axes() \%>\%
  gather_tunnel_data() \%>\%
  trim_tunnel_outliers() \%>\%
  rotate_tunnel() \%>\%
  get_velocity() \%>\%
  select_x_percent(desired_percent = 50)

visualize_frame_gap_choice(motive_selected, loops = 10)
}
\seealso{
Other data cleaning functions: 
\code{\link{gather_tunnel_data}()},
\code{\link{get_full_trajectories}()},
\code{\link{quick_separate_trajectories}()},
\code{\link{redefine_tunnel_center}()},
\code{\link{relabel_viewr_axes}()},
\code{\link{rename_viewr_characters}()},
\code{\link{rotate_tunnel}()},
\code{\link{select_x_percent}()},
\code{\link{separate_trajectories}()},
\code{\link{standardize_tunnel}()},
\code{\link{trim_tunnel_outliers}()}

Other plotting functions: 
\code{\link{plot_by_subject}()},
\code{\link{plot_viewr_trajectories}()}

Other functions that define or clean trajectories: 
\code{\link{get_full_trajectories}()},
\code{\link{quick_separate_trajectories}()},
\code{\link{separate_trajectories}()}
}
\author{
Melissa S. Armstrong and Vikram B. Baliga
}
\concept{data cleaning functions}
\concept{functions that define or clean trajectories}
\concept{plotting functions}
