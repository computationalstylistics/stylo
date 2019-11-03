
#################################################################
# A high-level function that ties a number of other functions responsible
# for uploading texts, deleting markup, sampling, splitting into n-grams, etc.
# It is build on top of a number of functions and thus it requires a large
# number of arguments. See:
#
# load.corpus(corpus.dir="",files)
# delete.markup(input.text,markup.type="plain")
# txt.to.words.ext(input.text,corpus.lang="English")
# make.samples(tokenized.input.data,sample.size=10000,sampling="no.sampling",
#   sampling.with.replacement=FALSE)
# txt.to.features(tokenized.text,features="w",ngram.size=1)
#
#################################################################

load.corpus.and.parse = function(files = "all",
         corpus.dir = "",
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


  # first, checking which files were requested; usually, the user is 
  # expected to specify a vector with samples' names
#  if(files[1] == "all") {
#  	  files = list.files(all.files = FALSE)
#  }

loaded.corpus = load.corpus(files = files,
                              corpus.dir = corpus.dir,
                              encoding = encoding)
  # dropping file extensions from sample names
  names(loaded.corpus) = gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)","",
                         names(loaded.corpus) )

  # deleting xml/html markup by applying the function "delete.markup"
  loaded.corpus = lapply(loaded.corpus, delete.markup, markup.type = markup.type)
  # deleting punctuation, splitting into words
  message("slicing input text into tokens...\n")
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
  message("\nturning words into features, e.g. char n-grams (if applicable)...")
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

