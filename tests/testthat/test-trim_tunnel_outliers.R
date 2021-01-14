## Tests of trim_tunnel_outliers() are in this file

test_that("trim_tunnel_outliers() fails when non-numerics are supplied", {
  expect_error(trim_tunnel_outliers("steve"))
  expect_error(trim_tunnel_outliers(c("a", "b", "c")))
  expect_error(trim_tunnel_outliers())
  expect_error(trim_tunnel_outliers(data.frame(rnorm(100))))
})

## Set up for tests of the function value return
## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                              package = 'pathviewr'))

## Clean the file up to gather()
motive_gathered <-
  motive_data %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data()

## Now trim outliers using default values
motive_trimmed <-
  motive_gathered %>%
  trim_tunnel_outliers(lengths_min = 0,
                       lengths_max = 3,
                       widths_min = -0.4,
                       widths_max = 0.8,
                       heights_min = -0.2,
                       heights_max = 0.5)

## Test input
test_that("trim_tunnel_outliers() handles arguments properly",{
  expect_error(trim_tunnel_outliers(motive_gathered[,-4]))
  expect_error(trim_tunnel_outliers(motive_gathered[,-5]))
  expect_error(trim_tunnel_outliers(motive_gathered[,-6]))
})
