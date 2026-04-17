test_that("stylo batch mode works with built-in frequency data", {
  data("lee", package = "stylo")
  root <- make_test_root()

  result <- suppressMessages(stylo(
    gui = FALSE,
    path = root,
    frequencies = lee,
    display.on.screen = FALSE,
    write.pdf.file = FALSE,
    write.jpg.file = FALSE,
    write.svg.file = FALSE,
    write.png.file = FALSE
  ))

  expect_s3_class(result, "stylo.results")
})

test_that("stylo(gui = TRUE) falls back cleanly when GUI support is unavailable", {
  data("lee", package = "stylo")
  root <- make_test_root()

  testthat::local_mocked_bindings(
    .stylo_gui_available = function() FALSE,
    gui.stylo = function(...) stop("unexpected gui call"),
    .package = "stylo"
  )

  result <- suppressMessages(stylo(
    gui = TRUE,
    path = root,
    frequencies = lee,
    display.on.screen = FALSE,
    write.pdf.file = FALSE,
    write.jpg.file = FALSE,
    write.svg.file = FALSE,
    write.png.file = FALSE
  ))

  expect_s3_class(result, "stylo.results")
})
