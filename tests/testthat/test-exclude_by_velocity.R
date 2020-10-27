## Tests of exclude_by_velocity() are in this file

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

## Import and clean the example Motive data
motive_import_and_clean <-
  suppressMessages(
    import_and_clean_viewr(
      file_name = system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'),
      desired_percent = 50,
      max_frame_gap = "autodetect",
      span = 0.95
    )
  )

motive_vel_filtered <-
  motive_import_and_clean %>%
  exclude_by_velocity(vel_min = 2)

test_that("exclude_by_velocity() fails when nonsense is supplied", {
  expect_error(exclude_by_velocity("steve"))
  expect_error(exclude_by_velocity(c("a", "b", "c")))
  expect_error(exclude_by_velocity())
  expect_error(exclude_by_velocity(data.frame(rnorm(100))))
  expect_error(exclude_by_velocity(motive_import_and_clean,
                                   vel_min = "6"))
  expect_error(exclude_by_velocity(motive_import_and_clean,
                                   vel_max = "6"))
})

test_that("exclude_by_velocity() produces correct output", {
  expect_equal(min(motive_vel_filtered[,12]),
               2.774914,
               tolerance = 1e-5)
})
