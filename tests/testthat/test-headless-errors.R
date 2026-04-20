test_that("gui.stylo() reports missing optional GUI support cleanly", {
  testthat::local_mocked_bindings(
    .stylo_gui_available = function() FALSE,
    .stylo_has_namespace = function(pkg) pkg != "tcltk2",
    .package = "stylo"
  )

  expect_error(gui.stylo(), "missing packages: tcltk2", fixed = TRUE)
})

test_that("stylo reports a plain error instead of opening a directory chooser", {
  root <- make_test_root()

  testthat::local_mocked_bindings(
    .stylo_dialogs_available = function() FALSE,
    tk_choose.dir = function(...) stop("unexpected chooser"),
    .package = "stylo"
  )

  expect_error(
    stylo(
      gui = FALSE,
      path = root,
      corpus.dir = "missing-corpus",
      interactive.files = FALSE
    ),
    "Working directory should contain the subdirectory",
    fixed = TRUE
  )
})

test_that("stylo reports a plain error instead of opening a file chooser", {
  root <- make_test_root()
  write_test_corpus(root, dir_name = "corpus")

  testthat::local_mocked_bindings(
    .stylo_dialogs_available = function() FALSE,
    tk_choose.files = function(...) stop("unexpected chooser"),
    .package = "stylo"
  )

  expect_error(
    stylo(
      gui = FALSE,
      path = root,
      corpus.dir = "corpus",
      interactive.files = TRUE
    ),
    "Interactive file selection requires Tcl/Tk.",
    fixed = TRUE
  )
})
