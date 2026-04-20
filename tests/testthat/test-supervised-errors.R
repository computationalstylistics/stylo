test_that("classify reports a plain error when corpora directories are missing", {
  root <- make_test_root()

  testthat::local_mocked_bindings(
    .stylo_dialogs_available = function() FALSE,
    tk_choose.dir = function(...) stop("unexpected chooser"),
    .package = "stylo"
  )

  expect_error(
    classify(
      gui = FALSE,
      path = root,
      training.corpus.dir = "train-set",
      test.corpus.dir = "test-set"
    ),
    "Working directory should contain two subdirectories",
    fixed = TRUE
  )
})

test_that("oppose reports a plain error when corpora directories are missing", {
  root <- make_test_root()

  testthat::local_mocked_bindings(
    .stylo_dialogs_available = function() FALSE,
    tk_choose.dir = function(...) stop("unexpected chooser"),
    .package = "stylo"
  )

  expect_error(
    oppose(
      gui = FALSE,
      path = root,
      primary.corpus.dir = "primary",
      secondary.corpus.dir = "secondary"
    ),
    "Working directory should contain two subdirectories",
    fixed = TRUE
  )
})
