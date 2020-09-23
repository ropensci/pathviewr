## Tests of get_3d_cross_prod() are in this file

## Test get_3d_cross_prod()
test_that("get_3d_cross_prod() fails when non-numerics are supplied", {
  expect_error(get_3d_cross_prod("steve"))
  expect_error(get_3d_cross_prod(c("a", "b", "c")))
})

## Set up vectors
v1 <- c(1, 1, 3)
v2 <- c(3, 1, 3)

test_that("get_3d_cross_prod() returns the correct value", {
  expect_equal(
    get_3d_cross_prod(v1, v2),
    c(0, 6, -2))
})
