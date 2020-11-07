## Tests of import_batch(), clean_batch() and import_and_clean_batch()

motive_test_data <-
  read_motive_csv(system.file("extdata", "pathviewR_motive_example_data.csv",
                              package = 'pathviewR'))

import_list <-
  c(rep(
    system.file("extdata", "pathviewR_motive_example_data.csv",
                package = 'pathviewR'),
    3
  ))

import_list_flydra <-
  c(rep(
    system.file("extdata", "pathviewR_flydra_example_data.mat",
                package = 'pathviewR'),
    3
  ))

## Batch import
motive_batch_imports <-
  import_batch(import_list,
               import_method = "motive",
               import_messaging = TRUE,
               simplify_marker_naming = TRUE)
flydra_batch_imports <-
  import_batch(import_list_flydra,
               import_method = "flydra",
               import_messaging = TRUE,
               subject_name = "bob",
               frame_rate = 100)

## Batch cleaning of these imported files
## via clean_viewr_batch()
motive_batch_cleaned <-
  clean_viewr_batch(
    file_announce = TRUE,
    motive_batch_imports,
    desired_percent = 50,
    max_frame_gap = "autodetect",
    span = 0.95
  )

## Alternatively, use import_and_clean_batch() to
## combine import with cleaning on a batch of files
motive_batch_import_and_clean <-
  import_and_clean_batch(
    import_list,
    import_method = "motive",
    import_messaging = TRUE,
    motive_batch_imports,
    desired_percent = 50,
    max_frame_gap = "autodetect",
    span = 0.95
  )

## Each of these lists of objects can be bound into
## one viewr object (i.e. one tibble) via
## bind_viewr_objects()
motive_bound_one <-
  bind_viewr_objects(motive_batch_cleaned)

motive_bound_two <-
  bind_viewr_objects(motive_batch_import_and_clean)

#### import_batch ####
test_that("import_batch() fails when no file is supplied",
          {
            expect_error(import_batch())
          })
test_that("import_batch() fails when import_method is wrong",
          {
            expect_error(import_batch(import_list,
                                      import_method = "magic"))
          })
test_that("import_batch() gives messages when import_messaging is true",
          {
            expect_message(import_batch(
              import_list,
              import_method = "motive",
              import_messaging = TRUE,
              simplify_marker_naming = TRUE
            ))
            expect_message(import_batch(
              import_list_flydra,
              import_method = "flydra",
              import_messaging = TRUE,
              subject_name = "bob"
            ))
          })

#### clean_viewr_batch ####
test_that("clean_viewr_batch() fails when input is nonsense",
          {
            expect_error(clean_viewr_batch())
            expect_error(clean_viewr_batch(motive_test_data))
          })
test_that("clean_viewr_batch() announces files",
          {
            expect_message(clean_viewr_batch(file_announce = TRUE,
                                           motive_batch_imports,
                                           desired_percent = 50,
                                           max_frame_gap = "autodetect",
                                           span = 0.95))
          })

#### import_and_clean_batch ####
test_that("import_and_clean_batch() fails when input is nonsense",
          {
            expect_error(import_and_clean_batch())
            expect_error(import_and_clean_batch(import_list,
                                                import_method = "magic"))
            expect_error(import_and_clean_batch(motive_test_data))
          })
test_that(
  "import_and_clean_batch() gives messages when import_messaging is true",
          {
            expect_message(import_and_clean_batch(
              import_list,
              import_method = "motive",
              import_messaging = TRUE,
              simplify_marker_naming = TRUE
            ))
            expect_error(import_and_clean_batch(
              import_list_flydra,
              import_method = "flydra",
              import_messaging = TRUE,
              subject_name = "bob",
              frame_rate = 100,
              gather_tunnel_data = FALSE
            ))
          })

#### bind_viewr_objects ####
test_that("bind_viewr_objects() fails when input is nonsense",
          {
            expect_error(bind_viewr_objects())
            expect_error(bind_viewr_objects(motive_test_data))
          })
test_that("bind_viewr_objects() binds data properly",
          {
            expect_equal(
              dim(bind_viewr_objects(motive_batch_cleaned)),
              c(1347, 24)
              )
          })
test_that("bind_viewr_objects() handles attributes",
          {
            expect_equal(
              attr(bind_viewr_objects(motive_batch_cleaned), "pathviewR_steps"),
              c("viewr", "bound_viewr_objects")
            )
          })
