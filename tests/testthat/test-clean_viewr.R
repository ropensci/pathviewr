## Tests of clean_viewr() and import_and_clean_viewr()

motive_test_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Import the example Flydra data included in the package
flydra_data <-
  read_flydra_mat(system.file("extdata", "pathviewR_flydra_example_data.mat",
                              package = 'pathviewR'),
                  subject_name = "birdie_wooster")

## run it through
motive_cleaned_renamed <-
  clean_viewr(
    motive_test_data,
    rename_viewr_characters = TRUE,
    target_column = "subject",
    pattern = "device",
    replacement = ""
  )

motive_cleaned_filled <-
  clean_viewr(
    motive_test_data,
    fill_traj_gaps = TRUE,
    loess_degree = 1,
    loess_criterion = c("aicc", "gcv"),
    loess_family = c("gaussian", "symmetric"),
    loess_user_span = NULL
  )

flydra_redefined_cleaned <-
  clean_viewr(
    flydra_data,
    relabel_viewr_axes = FALSE,
    gather_tunnel_data = FALSE,
    trim_tunnel_outliers = FALSE,
    standardization_option = "redefine_tunnel_center",
    length_method = "median",
    desired_percent = 0.5,
    max_frame_gap = 1,
    span = 0.9
  )

motive_import_and_clean <-
  import_and_clean_viewr(
    file_name = system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
    desired_percent = 50,
    max_frame_gap = "autodetect",
    span = 0.95
  )

motive_import_and_clean2 <-
  import_and_clean_viewr(
    file_name = system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
    fill_traj_gaps = TRUE,
    loess_degree = 1,
    loess_criterion = c("aicc", "gcv"),
    loess_family = c("gaussian", "symmetric"),
    loess_user_span = NULL
  )

motive_import_and_clean3 <-
  import_and_clean_viewr(
    file_name = system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
    rename_viewr_characters = TRUE,
    target_column = "subject",
    pattern = "device",
    replacement = ""
  )

test_that("a message is made even if inputs are good",
          {
            expect_message(
              motive_import_and_clean <-
                import_and_clean_viewr(
                  file_name =
                    system.file("extdata", "pathviewR_motive_example_data.csv",
                                package = 'pathviewR'),
                  desired_percent = 50,
                  max_frame_gap = "autodetect",
                  span = 0.95,
                  fill_traj_gaps = TRUE,
                  loess_degree = 1,
                  loess_criterion = c("aicc", "gcv"),
                  loess_family = c("gaussian", "symmetric"),
                  loess_user_span = NULL
                )
            )
          })


#### clean_viewr ####
## Pre-import tests
test_that("clean_viewr() fails when no file is supplied",
          {
            expect_error(clean_viewr())
          })
test_that("clean_viewr() fails when unrecognized arguments are supplied",
          {
            expect_error(clean_viewr(steve = "steve"))
          })


## Test that true/false checks fail successfully
test_that("clean_viewr() fails when relabel_viewr_axes has args but is FALSE",
          {
            expect_error(
              clean_viewr(
                motive_test_data,
                relabel_viewr_axes = FALSE,
                tunnel_length = "_z",
                tunnel_width = "_x",
                tunnel_height = "_y",
                real = "_w"
              )
            )
          })
test_that("clean_viewr() fails when gather_tunnel_data has args but is FALSE",
          {
            expect_error(clean_viewr(
              motive_test_data,
              gather_tunnel_data = FALSE,
              NA_drop = TRUE
            ))
          })
test_that("clean_viewr() fails when trim_tunnel_outliers has args but is FALSE",
          {
            expect_error(
              clean_viewr(
                motive_test_data,
                trim_tunnel_outliers = FALSE,
                lengths_min = 0,
                lengths_max = 3,
                widths_min = -0.4,
                widths_max = 0.8,
                heights_min = -0.2,
                heights_max = 0.5
              )
            )
          })
test_that("clean_viewr() fails when get_velocity has args but is FALSE",
          {
            expect_error(
              clean_viewr(
                motive_test_data,
                get_velocity = FALSE,
                time_col = "time_sec",
                length_col = "position_length",
                width_col = "position_width",
                height_col = "position_height"
              )
            )
          })
test_that("clean_viewr() fails when select_x_percent has args but is FALSE",
          {
            expect_error(clean_viewr(
              motive_test_data,
              select_x_percent = FALSE,
              desired_percent = 33
            ))
          })
test_that(
  "clean_viewr() fails when rename_viewr_characters has args but is FALSE",
          {
            expect_error(
              clean_viewr(
                motive_test_data,
                rename_viewr_characters = FALSE,
                target_column = "subject",
                pattern = "device",
                replacement = ""
              )
            )
          })
test_that(
  "clean_viewr() fails when separate_trajectories has args but is FALSE",
          {
            expect_error(
              clean_viewr(
                motive_test_data,
                separate_trajectories = FALSE,
                max_frame_gap = 1,
                frame_rate_proportion = 0.1
              )
            )
          })
test_that(
  "clean_viewr() fails when get_full_trajectories has args but is FALSE",
          {
            expect_error(clean_viewr(
              motive_test_data,
              get_full_trajectories = FALSE,
              span = 0.8
            ))
          })
test_that("clean_viewr() fails when fill_traj_gaps has args but is FALSE",
          {
            expect_error(
              clean_viewr(
                motive_test_data,
                fill_traj_gaps = FALSE,
                loess_degree = 1,
                loess_criterion = c("aicc", "gcv"),
                loess_family = c("gaussian", "symmetric")
              )
            )
          })
test_that("clean_viewr() fails when standardization option is nonsense",
          {
            expect_error(clean_viewr(motive_test_data,
                                     standardization_option = "help_pls"))
          })

## Test that unused params also fail successfully
test_that("clean_viewr() reports when unused params are given",
          {
            expect_message(clean_viewr(
              motive_test_data,
              stuff = "stuff",
              dog = "chewie"
            ))
          })

#### Import and clean viwer ####
## Pre-import tests
test_that("import_and_clean_viewr() fails when no file is supplied",
          {
            expect_error(import_and_clean_viewr())
          })
test_that("import_and_clean_viewr() fails when a false file is supplied",
          {
            expect_error(import_and_clean_viewr(file_name = "nope.csv"))
          })
test_that(
  "import_and_clean_viewr() fails when unrecognized arguments are supplied",
          {
            expect_error(import_and_clean_viewr(steve = "steve"))
          })


## Test that true/false checks fail successfully
test_that(
  "import_and_clean_viewr() fails when relabel_viewr_axes
  has args but is FALSE",
          {
            expect_error(
              import_and_clean_viewr(
                system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
                relabel_viewr_axes = FALSE,
                tunnel_length = "_z",
                tunnel_width = "_x",
                tunnel_height = "_y",
                real = "_w"
              )
            )
          })
test_that(
  "import_and_clean_viewr() fails when gather_tunnel_data
  has args but is FALSE",
          {
            expect_error(import_and_clean_viewr(
              system.file("extdata", "pathviewR_motive_example_data.csv",
                          package = 'pathviewR'),
              gather_tunnel_data = FALSE,
              NA_drop = TRUE
            ))
          })
test_that(
  "import_and_clean_viewr() fails when trim_tunnel_outliers
  has args but is FALSE",
          {
            expect_error(
              import_and_clean_viewr(
                system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
                trim_tunnel_outliers = FALSE,
                lengths_min = 0,
                lengths_max = 3,
                widths_min = -0.4,
                widths_max = 0.8,
                heights_min = -0.2,
                heights_max = 0.5
              )
            )
          })
test_that(
  "import_and_clean_viewr() fails when get_velocity
  has args but is FALSE",
          {
            expect_error(
              import_and_clean_viewr(
                system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
                get_velocity = FALSE,
                time_col = "time_sec",
                length_col = "position_length",
                width_col = "position_width",
                height_col = "position_height"
              )
            )
          })
test_that(
  "import_and_clean_viewr() fails when select_x_percent
  has args but is FALSE",
          {
            expect_error(
              import_and_clean_viewr(
                system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
                select_x_percent = FALSE,
                desired_percent = 33
              )
            )
          })
test_that(
  "import_and_clean_viewr() fails when rename_viewr_characters
  has args but is FALSE",
          {
            expect_error(
              import_and_clean_viewr(
                system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
                rename_viewr_characters = FALSE,
                target_column = "subject",
                pattern,
                replacement = ""
              )
            )
          })
test_that(
  "import_and_clean_viewr() fails when separate_trajectories
  has args but is FALSE",
          {
            expect_error(
              import_and_clean_viewr(
                system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
                separate_trajectories = FALSE,
                max_frame_gap = 1,
                frame_rate_proportion = 0.1
              )
            )
          })
test_that(
  "import_and_clean_viewr() fails when get_full_trajectories
  has args but is FALSE",
          {
            expect_error(import_and_clean_viewr(
              system.file("extdata", "pathviewR_motive_example_data.csv",
                          package = 'pathviewR'),
              get_full_trajectories = FALSE,
              span = 0.8
            ))
          })
test_that(
  "import_and_clean_viewr() fails when fill_traj_gaps
  has args but is FALSE",
          {
            expect_error(
              import_and_clean_viewr(
                system.file("extdata", "pathviewR_motive_example_data.csv",
                            package = 'pathviewR'),
                fill_traj_gaps = FALSE,
                loess_degree = 1,
                loess_criterion = c("aicc", "gcv"),
                loess_family = c("gaussian", "symmetric")
              )
            )
          })
## Test that unused params also fail successfully
test_that("import_and_clean_viewr() reports when unused params are given",
          {
            expect_message(import_and_clean_viewr(
              system.file("extdata", "pathviewR_motive_example_data.csv",
                          package = 'pathviewR'),
              stuff = "stuff",
              dog = "chewie"
            ))
          })
## Test that standardization option is correctly enforced
test_that(
  "import_and_clean_viewr() fails when standardization option is nonsense",
          {
            expect_error(import_and_clean_viewr(
              system.file("extdata", "pathviewR_motive_example_data.csv",
                          package = 'pathviewR'),
              standardization_option = "chewie"
            ))
          })
