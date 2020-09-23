# Tests of rm_by_trajnum()

# pre-import tests
test_that("rm_by_trajnum() fails when non-numerics are supplied", {
  expect_error(rm_by_trajnum("steve"))
  expect_error(rm_by_trajnum(c("a", "b", "c")))
})

# Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

#Clean the file. Recommended to use full pathviewR pipeline before rm_by_trajnum()
motive_full <-
  motive_data %>%
  clean_viewr(desired_percent = 50,
              max_frame_gap = "autodetect",
              span = 0.95)

#Remove subjects that have not completed at least 150 trajectories:
motive_rm_unmirrored <-
  motive_full %>%
  rm_by_trajnum(trajnum = 150)

#test unmirrored output
test_that("rm_by_trajnum() counts trajectories correctly", {
  expect_equal(rm_by_trajnum(motive_full, trajnum = 150)[1, 25]$n, 223)
  expect_equal(rm_by_trajnum(motive_full, trajnum = 150)[187, 25]$n, 223)
})

test_that("rm_by_trajnum() removes subjects w/n < trajnum", {
  expect_equal(unique(rm_by_trajnum(motive_full, trajnum = 150)$subject), "device02")
})

# Add treatment information
motive_full$treatment <- c(rep("latA", 100),
                           rep("latB", 100),
                           rep("latA", 100),
                           rep("latB", 149))

# Remove subjects by that have not completed at least 10 trajectories in
# both treatments
motive_rm_mirrored <-
  motive_full %>%
  rm_by_trajnum(
    trajnum = 10,
    mirrored = TRUE,
    treatment1 = "latA",
    treatment2 = "latB"
  )

#test mirrored output
test_that("rm_by_trajnum() removes mirrored subjects w/n < trajnum", {
  expect_equal(unique(rm_by_trajnum(motive_full, trajnum = 10, mirrored = TRUE,
                            treatment1 = "latA", treatment2 = "latB")$subject),
               c("device02", "device03"))
})
