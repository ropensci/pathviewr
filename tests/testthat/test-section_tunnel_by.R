## Tests of section_tunnel_by() are in this file

## Import the Flydra example data included in
## the package
flydra_data <-
  read_flydra_mat(
    system.file("extdata",
                "pathviewr_flydra_example_data.mat",
                package = 'pathviewr'),
    subject_name = "birdie_wooster"
  )

## Load data and run section_tunnel_by()
test_mat <-
  read_flydra_mat(system.file("extdata", "pathviewr_flydra_example_data.mat",
                              package = 'pathviewr'),
                  subject_name = "birdie_wooster") %>%
  redefine_tunnel_center(length_method = "middle",
                         height_method = "user-defined",
                         height_zero = 1.44) %>%
  select_x_percent(desired_percent = 50) %>%
  separate_trajectories(max_frame_gap = 1) %>%
  get_full_trajectories(span = 0.95) %>%
  section_tunnel_by(number_of_sections = 10)

test_mat2 <-
  read_flydra_mat(system.file("extdata", "pathviewr_flydra_example_data.mat",
                              package = 'pathviewr'),
                  subject_name = "birdie_wooster") %>%
  redefine_tunnel_center(length_method = "middle",
                         height_method = "user-defined",
                         height_zero = 1.44) %>%
  select_x_percent(desired_percent = 50) %>%
  separate_trajectories(max_frame_gap = 1) %>%
  get_full_trajectories(span = 0.95)

test_that("section_tunnel_by() fails when nonsense is supplied", {
  expect_error(section_tunnel_by("steve"))
  expect_error(section_tunnel_by(c("a", "b", "c")))
  expect_error(section_tunnel_by())
  expect_error(section_tunnel_by(data.frame(rnorm(100))))
  expect_error(section_tunnel_by(test_mat2,
                                 axis = "what"))
})

test_that("section_tunnel_by() produces correct output", {
  expect_equal(as.numeric(test_mat[1,16]),
               10,
               tolerance = 1e-5)
})
