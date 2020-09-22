## Tests of both rad_2_deg() and deg_2_rad() are in this file

## Test rad_2_deg()
test_that("rad_2_deg() fails when non-numerics are supplied", {
  expect_error(rad_2_deg("steve"))
  expect_error(rad_2_deg(c("a", "b", "c")))
})

test_that("rad_2_deg() returns the correct value", {
  expect_equal(rad_2_deg(pi), 180)
  expect_equal(rad_2_deg(1), 57.29578, tolerance = 1e-3)
})


## Test deg_2_rad()
test_that("deg_2_rad() fails when non-numerics are supplied", {
  expect_error(deg_2_rad("steve"))
  expect_error(deg_2_rad(c("a", "b", "c")))
})

test_that("deg_2_rad() returns the correct value", {
  expect_equal(deg_2_rad(180), 3.141593, tolerance = 1e-3)
  expect_equal(deg_2_rad(1), 0.01745329, tolerance = 1e-3)
})
