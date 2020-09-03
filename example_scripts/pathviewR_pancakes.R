## Last updated: 2020-09-03 VBB

## Script for testing things out as functions are written and showcasing worked
## examples.

############################### package loading ################################
## Specify the packages you'll use in the script
packages <-
  c(
    "devtools",
    "tidyverse",
    "readxl",
    "R.matlab",
    "ggthemes",
    "gridExtra",
    "data.table",
    "rgl"
  )
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)}})

## Source un-exported pathviewR things too
devtools::load_all()


############################## quick data import ###############################
motive_data_path <- './inst/extdata/pathviewR_motive_example_data.csv'

motive_data_all_defaults <-
  motive_data_path %>% import_and_clean_viewr()

motive_data_mfg_autodetect <-
  motive_data_path %>% import_and_clean_viewr(max_frame_gap = "autodetect",
                                         frame_gap_messaging = TRUE)

## "Full" version, using select x = 50
motive_data_full <-
  motive_data_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  get_velocity() %>%
  select_x_percent(desired_percent = 50) %>%
  ## skip rename_viewr_characters(), which defaults to FALSE anyway
  separate_trajectories(max_frame_gap = "autodetect",
                        frame_gap_messaging = TRUE) %>%
  get_full_trajectories(span = 0.95)
attr(motive_data_full, "pathviewR_steps")
plot(motive_data_full$position_length,
     motive_data_full$position_width,
     asp = 1, col = as.factor(motive_data_full$file_sub_traj))

## Defaults, done explicitly:
motive_data_all_defaults_explicit <-
  motive_data_path %>%
  read_motive_csv() %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data() %>%
  trim_tunnel_outliers() %>%
  rotate_tunnel() %>%
  get_velocity() %>%
  select_x_percent() %>%
  ## skip rename_viewr_characters(), which defaults to FALSE anyway
  separate_trajectories() %>%
  get_full_trajectories()

## Are motive_data_all_defaults and motive_data_all_defaults_explicit the same?
identical(motive_data_all_defaults, motive_data_all_defaults_explicit)


#################################### roz2016 ###################################
## Test pancake
test_mat <-
  read_flydra_mat(
    "./inst/extdata/pathviewR_flydra_example_data.mat",
    subject_name = "steve"
  ) %>%
  redefine_tunnel_center(length_method = "middle",
                         height_method = "user-defined",
                         height_zero = 1.44) %>%
  select_x_percent(desired_percent = 50) %>%
  separate_trajectories(max_frame_gap = 1) %>%
  get_full_trajectories(span = 0.95) %>%
  section_tunnel_by(10)

## 2D overhead
plot(test_mat$position_length,
     test_mat$position_width,
     asp = 1, col = as.factor(test_mat$file_sub_traj))

## 3D plot
library(scales)
n <- length(unique(test_mat$file_sub_traj))
dat.col <- data.frame(grps = unique(test_mat$file_sub_traj),
                      colz = rainbow(n))  ## you can also use  brewer_pal()(n)
trj <- tibble(grps = test_mat$file_sub_traj)
data_col <- left_join(trj, dat.col)
rgl::plot3d(x = test_mat$position_length,
            y = test_mat$position_width,
            z = test_mat$position_height,
            col = data_col$colz);aspect3d("iso")


##### Quaternions maybe? #####
library(RSpincalc)

# S <- 1
# L <- 52 # length; using first trajectory only (rightwards)
S <- 53
L <- 98 #(leftwards)

test_pos <- data.frame(position_length = motive_data_full$position_length[S:L],
                       position_width =  motive_data_full$position_width[S:L],
                       position_height = motive_data_full$position_height[S:L])

rgl::plot3d(test_pos$position_length,
            test_pos$position_width,
            test_pos$position_height,
            ylim = c(-0.5, 0.5),
            zlim = c(-0.5, 0));aspect3d("iso")

test_quat <- cbind(motive_data_full$rotation_real[S:L],
                   motive_data_full$rotation_length[S:L],
                   motive_data_full$rotation_width[S:L],
                   motive_data_full$rotation_height[S:L])

test_quat2eulangl <- RSpincalc::Q2EA(test_quat, EulerOrder = "xyz")
test_quat2eulangl_deg <- apply(test_quat2eulangl, MARGIN = 2, FUN = rad_2_deg)
test_quat2eulvec <- RSpincalc::Q2EV(test_quat)


alltogether <- data.frame(position_length = motive_data_full$position_length[S:L],
                          position_width =  motive_data_full$position_width[S:L],
                          position_height = motive_data_full$position_height[S:L],
                          motive_data_full$rotation_real[S:L],
                          motive_data_full$rotation_length[S:L],
                          motive_data_full$rotation_width[S:L],
                          motive_data_full$rotation_height[S:L],
                          test_quat2eulangl_deg,
                          test_quat2eulvec)

lattice::splom(alltogether[,c(1:3,11:13)])


alltogether$len_trans <- alltogether$position_length + (alltogether$X1.1/3)
alltogether$wid_trans <- alltogether$position_width + (alltogether$X2.1/3)
alltogether$hei_trans <- alltogether$position_height + (alltogether$X3.1/3)



as.matrix(test_pos) -> M
colnames(M) <- NULL

as.matrix(cbind(alltogether$len_trans,
                alltogether$wid_trans,
                alltogether$hei_trans)) -> Tr


rgl::plot3d(x = M[,1],
            y = M[,2],
            z = M[,3],
            xlim = c(-1, 1),
            ylim = c(-0.5, 0.5),
            zlim = c(-0.5, 0.5));aspect3d("iso")
arrow3d(M[1,1:3], Tr[1,1:3], type = "lines", col = "red")
arrow3d(M[10,1:3], Tr[10,1:3], type = "lines", col = "orange")
arrow3d(M[15,1:3], Tr[15,1:3], type = "lines", col = "yellow")
arrow3d(M[20,1:3], Tr[20,1:3], type = "lines", col = "green")
arrow3d(M[30,1:3], Tr[30,1:3], type = "lines", col = "blue")
arrow3d(M[35,1:3], Tr[35,1:3], type = "lines", col = "grey30")
arrow3d(M[36,1:3], Tr[36,1:3], type = "lines", col = "black")
arrow3d(M[37,1:3], Tr[37,1:3], type = "lines", col = "grey70")
arrow3d(M[50,1:3], Tr[50,1:3], type = "lines", col = "purple")
