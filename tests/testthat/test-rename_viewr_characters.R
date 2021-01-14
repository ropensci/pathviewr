## Tests of rename_viewr_characters() are in this file

## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                              package = 'pathviewr'))

## Clean the file. It is generally recommended to clean up to the
## "gather" step before running rescale_tunnel_data().
motive_gathered <-
  motive_data %>%
  relabel_viewr_axes() %>%
  gather_tunnel_data()

test_that("rename_viewr_characters() fails when nonsense is supplied", {
  expect_error(rename_viewr_characters("steve"))
  expect_error(rename_viewr_characters(c("a", "b", "c")))
  expect_error(rename_viewr_characters())
  expect_error(rename_viewr_characters(data.frame(rnorm(100))))
  expect_error(rename_viewr_characters(motive_gathered,
                                       target_column = "bob"))
  expect_error(rename_viewr_characters(motive_gathered[,-3]))
})

motive_renamed <-
  rename_viewr_characters(motive_gathered,
                          target_column = "subject",
                          pattern = "device",
                          replacement = "subject")

test_that(
  "rename_viewr_characters() replaces names properly", {
    expect_equal(as.character(motive_renamed[1,3]), "subject02")
  }
)
