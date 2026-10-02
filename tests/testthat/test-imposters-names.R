make_imposters_data <- function() {
  set.seed(1)
  ref <- matrix(runif(12 * 40), nrow = 12,
                dimnames = list(paste0(rep(c("A", "B", "C", "D"), each = 3), "_", 1:3),
                                paste0("w", 1:40)))
  cand <- runif(40)
  names(cand) <- colnames(ref)
  test <- runif(40)
  names(test) <- colnames(ref)
  list(ref = ref, cand = cand, test = test)
}

run_imposters <- function(ref, test, cand) {
  set.seed(2)
  suppressMessages(imposters(ref, test = test, candidate.set = cand, iterations = 100))
}

test_that("imposters gives the same score for named, reordered and unnamed vectors", {
  d <- make_imposters_data()
  named <- run_imposters(d$ref, d$test, d$cand)

  expect_equal(unname(named), 0.04)
  expect_equal(run_imposters(d$ref, d$test, unname(d$cand)), named)
  expect_equal(run_imposters(d$ref, unname(d$test), d$cand), named)
  expect_equal(run_imposters(d$ref, rev(d$test), rev(d$cand)), named)
  expect_equal(run_imposters(d$ref, rbind(rev(d$test)), d$cand), named)
})

test_that("imposters reports names assigned to unnamed vectors", {
  d <- make_imposters_data()
  msgs <- capture_messages(
    imposters(d$ref, test = unname(d$test), candidate.set = unname(d$cand), iterations = 1)
  )

  expect_match(msgs, "anonymous sample has no names", all = FALSE)
  expect_match(msgs, "candidate text has no names", all = FALSE)
})

test_that("imposters rejects vectors whose names do not match the reference set", {
  d <- make_imposters_data()
  bad_cand <- d$cand
  names(bad_cand)[40] <- "x40"
  bad_test <- d$test
  names(bad_test) <- paste0("x", 1:40)

  expect_error(suppressMessages(imposters(d$ref, test = d$test, candidate.set = bad_cand)),
               "names of the candidate text")
  expect_error(suppressMessages(imposters(d$ref, test = bad_test, candidate.set = d$cand)),
               "names of the anonymous sample")
})
