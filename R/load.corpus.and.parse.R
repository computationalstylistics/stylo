
#################################################################
# A high-level function that ties up a number of other functions responsible
# for uploading texts, deleting markup, sampling, splitting into n-grams, etc.
# It is build on top of a number of functions and thus it requires a large
# number of arguments. See:
#
# load.corpus(corpus.dir="",files)
# delete.markup(input.text,markup.type="plain")
# txt.to.words.ext(input.text,language="English")
# make.samples(tokenized.input.data,sample.size=10000,sampling="no.sampling",
#   sampling.with.replacement=FALSE
# txt.to.features(tokenized.text,features="w",ngram.size=1)
#
#################################################################

load.corpus.and.parse <-
function(files,
         corpus.dir="",
         markup.type="plain",
         language="English",
         sample.size=10000,
         sampling="no.sampling",
         sampling.with.replacement=FALSE,
         features="w",
         ngram.size=1) {

  loaded.corpus = load.corpus(files=files,corpus.dir=corpus.dir)
  # dropping file extensions from sample names
  names(loaded.corpus) = gsub("(\\.txt$)||(\\.xml$)||(\\.html$)||(\\.htm$)","",
                         names(loaded.corpus) )
  # deleteing xml/html markup by applying the function "delete.markup"
  loaded.corpus = lapply(loaded.corpus,delete.markup,markup.type=markup.type)
  # deleting punctuation, splitting into words
  cat("slicing input text into single words...\n")
  loaded.corpus = lapply(loaded.corpus,txt.to.words.ext,language=language)
  # normal sampling (if applicable); random sampling will be run later
  if(sampling == "normal.sampling") {
    loaded.corpus = 
      make.samples(loaded.corpus,sample.size,sampling,sampling.with.replacement)
    }
  # split into chars (if applicable), agglutinate into n-grams
  # [it takes a good while when char n-grams are chosen]
  cat("turning words into features, e.g. char n-grams (if applicable)...\n")
  loaded.corpus = lapply(loaded.corpus,txt.to.features,
                         features=features,ngram.size=ngram.size)
  # optionally, excerpt randomly a number of features from original data
  if(sampling == "random.sampling") {
    loaded.corpus = 
      make.samples(loaded.corpus,sample.size,sampling,sampling.with.replacement)
    }
return(loaded.corpus)
}

