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
