## Tests of get_dist_point_line() and find_curve_elbow() are in this file

## Test get_dist_point_line()
test_that("get_dist_point_line() fails when a mix of 2D and 3D coords are
          supplied", {
  expect_error(
    get_dist_point_line(
      point = c(0, 0),
      line_coord1 = c(0, 0, 0),
      line_coord2 = c(1, 1, 1)
    ))
})

test_that("get_dist_point_line() returns the correct value, 2D case", {
  expect_equal(
    get_dist_point_line(
      point = c(0, 0),
      line_coord1 = c(1, 0),
      line_coord2 = c(1, 5)
    ),
    1)
})
test_that("get_dist_point_line() returns the correct value, 3D case", {
  expect_equal(
    get_dist_point_line(
      point = c(0, 0, 0),
      line_coord1 = c(1, 0, 0),
      line_coord2 = c(1, 5, 0)
    ),
    1)
})


## Test find_curve_elbow()
test_that("find_curve_elbow() fails when non-numerics are supplied", {
  expect_error(find_curve_elbow("steve"))
  expect_error(find_curve_elbow(c("a", "b", "c")))
})

## Set up data frame
df <- data.frame(x = seq(1:10),
                 y = 1/seq(1:10))

test_that("find_curve_elbow() returns the correct value", {
  expect_equal(
    find_curve_elbow(df, plot_curve = TRUE),
    3)
})
