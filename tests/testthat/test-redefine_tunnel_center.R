## Tests of redefine_tunnel_center() are in this file

## Import the Flydra example data included in
## the package
flydra_data <-
  read_flydra_mat(
    system.file("extdata",
                "pathviewr_flydra_example_data.mat",
                package = 'pathviewr'),
    subject_name = "birdie_wooster"
  )

## Re-center the Flydra data set.
## Width will be untouched
## Length will use the "middle" definition
## And height will be user-defined to be
## zeroed at 1.44 on the original axis
flydra_centered <-
  flydra_data %>%
  redefine_tunnel_center(
    length_method = "middle",
    height_method = "user-defined",
    height_zero = 1.44
  )

flydra_middle <-
  flydra_data %>%
  redefine_tunnel_center(
    length_method = "middle",
    width_method = "middle",
    height_method = "middle"
  )

flydra_median <-
  flydra_data %>%
  redefine_tunnel_center(
    length_method = "median",
    width_method = "median",
    height_method = "median"
  )

flydra_original <-
  flydra_data %>%
  redefine_tunnel_center(
    length_method = "original",
    width_method = "original",
    height_method = "original"
  )

flydra_ud <-
  flydra_data %>%
  redefine_tunnel_center(
    length_method = "user-defined",
    length_zero = 1.35,
    width_method = "user-defined",
    width_zero = -0.17,
    height_method = "user-defined",
    height_zero = 1.30
  )

test_that("redefine_tunnel_center() fails when nonsense is supplied", {
  expect_error(redefine_tunnel_center("steve"))
  expect_error(redefine_tunnel_center(c("a", "b", "c")))
  expect_error(redefine_tunnel_center())
  expect_error(redefine_tunnel_center(data.frame(rnorm(100))))
  expect_error(redefine_tunnel_center(flydra_data[,-4]))
  expect_error(redefine_tunnel_center(flydra_data[,-5]))
  expect_error(redefine_tunnel_center(flydra_data[,-6]))
  expect_error(redefine_tunnel_center(flydra_data,
                                      length_method = 5))
  expect_error(redefine_tunnel_center(flydra_data,
                                      height_method = 5))
  expect_error(redefine_tunnel_center(flydra_data,
                                      width_method = 5))
  expect_error(redefine_tunnel_center(flydra_data,
                                      length_method = "user_defined"))
  expect_error(redefine_tunnel_center(flydra_data,
                                      width_method = "user_defined"))
  expect_error(redefine_tunnel_center(flydra_data,
                                      height_method = "user_defined"))
  expect_error(redefine_tunnel_center(flydra_data,
                                      length_method = "user_defined",
                                      length_zero = "a"))
  expect_error(redefine_tunnel_center(flydra_data,
                                      length_method = "user_defined",
                                      length_zero = NA))
  expect_error(redefine_tunnel_center(flydra_data,
                                      width_method = "user_defined",
                                      width_zero = "a"))
  expect_error(redefine_tunnel_center(flydra_data,
                                      width_method = "user_defined",
                                      width_zero = NA))
  expect_error(redefine_tunnel_center(flydra_data,
                                      height_method = "user_defined",
                                      height_zero = "a"))
  expect_error(redefine_tunnel_center(flydra_data,
                                      height_method = "user_defined",
                                      height_zero = NA))
  expect_error(redefine_tunnel_center(flydra_data,
                                      height_method = "user_defined"))
})

test_that("redefine_tunnel_center() produces correct output", {
  expect_equal(as.numeric(flydra_centered[1,4]),
               -1.256542,
               tolerance = 1e-5)
  expect_equal(as.numeric(flydra_middle[1,4]),
               -1.256542,
               tolerance = 1e-5)
  expect_equal(as.numeric(flydra_median[1,4]),
               0.01838291,
               tolerance = 1e-5)
  expect_equal(as.numeric(flydra_original[1,4]),
               0.8688741,
               tolerance = 1e-5)
  expect_equal(as.numeric(flydra_ud[1,4]),
               -0.4811259,
               tolerance = 1e-5)
})
