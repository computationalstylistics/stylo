

summary.stylo.corpus <- function(object, ...) {


  cat("\n")
  cat("Function call:\n")
  cat(deparse(attr(object, "call")), "\n")

  no.of.samples = length(object)
  no.of.fields  = sapply(object, length)
  names.of.samples = names(object)

  # cutting long filenames to their first 25 chars
  names.of.samples = paste(substr(names.of.samples,1,25),"...",sep="")

  cat("\n")
  cat("Number of texts/samples:        ", no.of.samples, "\n")
  cat("Total number of units:          ", sum(no.of.fields), "\n")
  cat("Number of units in samples:\n")
  cat("\n")

  # reducing the number of samples to be shown:
  if(no.of.samples > 10) {
    max.lines = 10
  } else {
    max.lines = no.of.samples
  }

  for(i in 1:max.lines) {
    cat( format(names.of.samples[i],width=31,justify="right"),
                      "  ", no.of.fields[i], "\n", sep="")
  }
  # if the size was reduced, add elipsis signs:
  if(no.of.samples > 10) {
    cat( format("...",width=31,justify="right"),
                      "  ", "...", "\n", sep="")
  }

  cat("\n")
  cat("Depending if the corpus has been tokenized or not, the \"units\" mean\n")
  cat("tokens (words, word pairs, POS-tags, character n-grams, etc.),\n")
  cat("or strings of text (usually paragraphs) ending with a newline char.\n")
  cat("\n")





# summary.stylotable
# no.of.rows:
# no.of.cols:
# first rows' names, etc.

# print.stylotable
# a description + the beginning of the table

# print.stylocorpus
# if biggish, display the beginnings 


}

