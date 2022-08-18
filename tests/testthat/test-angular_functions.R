## Tests of get_2d_angle() and get_3d_angle() are in this file

## Test get_2d_angle()
test_that("get_2d_angle() fails when non-numerics are supplied", {
  expect_error(get_2d_angle("steve"))
  expect_error(get_2d_angle(c("a", "b", "c")))
})

test_that("get_2d_angle() returns the correct value", {
  expect_equal(
    get_2d_angle(
      0, 1,
      0, 0,
      1, 0),
    90)
})

x1 <- c(1, 0, 1, 2)
y1 <- c(1, 1, 0, 1)
x2 <- c(0, 0, 0, 0)
y2 <- c(0, 0, 0, 0)
x3 <- c(1, 1, 0, 2)
y3 <- c(0, 0, 1, 0)


test_that("get_2d_angle() works with vectors", {
  expect_equal(
    get_2d_angle(
      x1 = x1, y1 = y1,
      x2 = x2, y2 = y2,
      x3 = x3, y3 = y3),
    c(45.00000, 90.00000, 90.00000, 26.56505), tolerance = 1e-5)
})


## Test get_3d_angle()
test_that("get_3d_angle() fails when non-numerics are supplied", {
  expect_error(get_3d_angle("steve"))
  expect_error(get_3d_angle(c("a", "b", "c")))
})

test_that("get_3d_angle() returns the correct value", {
  expect_equal(
    get_3d_angle(
      0, 1, 0,
      0, 0, 0,
      1, 0, 0),
    90)
})
