## Tests of redefine_tunnel_center() are in this file

## Import the Flydra example data included in
## the package
flydra_data <-
  read_flydra_mat(
    system.file("extdata",
                "pathviewR_flydra_example_data.mat",
                package = 'pathviewR'),
    subject_name = "birdie_wooster"
  )

## Re-center the Flydra data set.
## Width will be untouched
## Length will use the "middle" definition
## And height will be user-defined to be
## zeroed at 1.44 on the original axis
flydra_centered <-
  flydra_data %>%
  redefine_tunnel_center(length_method = "middle",
                         height_method = "user-defined",
                         height_zero = 1.44)


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
})
