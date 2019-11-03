
#################################################################
# A high-level function that ties a number of other functions responsible
# for deleting markup, sampling, splitting into n-grams, etc.
# It is build on top of a number of functions and thus it requires a large
# number of arguments. See:
#
# delete.markup(input.text,markup.type="plain")
# txt.to.words.ext(input.text,language="English")
# make.samples(tokenized.input.data,sample.size=10000,sampling="no.sampling",
#   sampling.with.replacement=FALSE)
# txt.to.features(tokenized.text,features="w",ngram.size=1)
#
#################################################################

parse.corpus = function(input.data,
         markup.type = "plain",
         corpus.lang = "English",
         splitting.rule = NULL,
         sample.size = 10000,
         sampling = "no.sampling",
         sample.overlap = 0,
         number.of.samples = 1,
         sampling.with.replacement = FALSE,
         features = "w",
         ngram.size = 1,
         preserve.case = FALSE,
         encoding = "UTF-8") {


  # sanitazing the dataset
  if(is.list(input.data) == FALSE) {
          loaded.corpus = list(input.data)
  } else {
          loaded.corpus = input.data
  }
  
  # adding names to the texts, if applicable
  if(is.null(names(loaded.corpus))) {
          names(loaded.corpus) = 1 : length(loaded.corpus)
  }

  


  # deleting xml/html markup by applying the function "delete.markup"
  loaded.corpus = lapply(loaded.corpus, delete.markup, markup.type = markup.type)
  # deleting punctuation, splitting into words
  cat("slicing input text into tokens...\n")
  loaded.corpus = lapply(loaded.corpus, txt.to.words.ext,
                                        corpus.lang = corpus.lang,
                                        splitting.rule = splitting.rule,
                                        preserve.case = preserve.case)
  # normal sampling (if applicable); random sampling will be run later
  if(sampling == "normal.sampling") {
    loaded.corpus = make.samples(loaded.corpus,
                                 sample.size,
                                 sampling,
                                 sample.overlap)
  }
  # split into chars (if applicable), agglutinate into n-grams
  # [it takes a good while when char n-grams are chosen]
  cat("turning words into features, e.g. char n-grams (if applicable)...\n")
  loaded.corpus = lapply(loaded.corpus, txt.to.features,
                         features = features, ngram.size = ngram.size)
  # optionally, excerpt randomly a number of features from original data
  if(sampling == "random.sampling") {
    loaded.corpus = make.samples(loaded.corpus,
                                 sample.size,
                                 sampling,
                                 sample.overlap = 0,
                                 number.of.samples,
                                 sampling.with.replacement)
  }

  # assigning a class
  class(loaded.corpus) = "stylo.corpus"
  # adding some information about the current function call
  # to the final list of results
  attr(loaded.corpus, "call") = match.call()

# returning the value
return(loaded.corpus)
}

