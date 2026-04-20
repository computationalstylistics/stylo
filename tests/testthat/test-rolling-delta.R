test_that("rolling.delta(gui = FALSE) does not enter GUI code", {
  root <- make_test_root()

  testthat::local_mocked_bindings(
    .Tcl = function(...) stop("unexpected gui call"),
    .package = "stylo"
  )

  expect_error(
    rolling.delta(gui = FALSE, path = root),
    "Corpus prepared incorrectly",
    fixed = TRUE
  )
})
