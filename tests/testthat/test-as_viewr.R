## Tests of as_viewr()

## Pre-import tests
test_that(
  "as_viewr() fails when nothing is supplied",
  {
    expect_error(as_viewr())
    expect_error(as_viewr(matrix()))
    expect_error(as_viewr("bob"))
  }
)

## Set up test data.frame
df <- data.frame(frame = seq(1, 100, by = 1),
                 time_sec = seq(0, by = 0.01, length.out = 100),
                 subject = "birdie_sanders",
                 z = rnorm(100),
                 x = rnorm(100),
                 y = rnorm(100))

## Use as_viewr() to convert it into a viewr object
asviewr_example <-
  as_viewr(
    df,
    frame_rate = 100,
    frame_col = 1,
    time_col = 2,
    subject_col = 3,
    position_length_col = 5,
    position_width_col = 6,
    position_height_col = 4
  )

## Test that imported files have the correct structure
test_that("as_viewr() interprets data in correctly", {
  ## Check that the column names appear correctly
  expect_equal(names(asviewr_example), c("frame", "time_sec", "subject",
                                          "position_length", "position_width",
                                          "position_height"))
  ## Check that all data were imported
  expect_equal(dim(asviewr_example), c(100, 6))
})

