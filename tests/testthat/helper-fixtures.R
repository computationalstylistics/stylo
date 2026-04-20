make_test_root <- function() {
  root <- tempfile("stylo-test-")
  dir.create(root)
  root
}

write_test_corpus <- function(root, dir_name = "corpus", n_files = 2,
                              n_words = 20) {
  corpus_path <- file.path(root, dir_name)
  dir.create(corpus_path, recursive = TRUE, showWarnings = FALSE)

  for (idx in seq_len(n_files)) {
    file_name <- sprintf("author%d_sample%d.txt", idx, idx)
    file_path <- file.path(corpus_path, file_name)
    file_text <- paste(rep(sprintf("token%d", idx), n_words), collapse = " ")
    writeLines(file_text, file_path)
  }

  corpus_path
}
