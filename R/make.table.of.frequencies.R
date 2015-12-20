
# #################################################
# FUNCTION: make.table.of.frequencies
# preparing a huge table with all the frequencies.
# Two arguments are required: (1) text data: either 
# a tokenized corpus (list), or a single text (vector); 
# (2) a vector containig the words (a reference word list)
# to rearrange particular frequency list in accordance
# to this vector. Optional argument: (3) absent.sensitive
# which is used to prevent building tables of non-existing
# words. When switched on, the variables that contain but O
# in all samples, will be excluded. However, in some cases
# is is important to keep all the variables regardless of
# their values. It is the case of comparing two corpora:
# even if a given word did not occur in the whole corpus A,
# it might have occured in the corpus B. In short: if 
# you perform any analysis involving I and II set, switch
# this option to FALSE.
# #################################################

make.table.of.frequencies = function(corpus, 
                                     features, 
                                     absent.sensitive = TRUE, 
                                     relative = TRUE) {


  # factorizing the features: this increases the speed radically:
  current.levels = features
  features = factor(features, levels = current.levels, ordered = TRUE)

  # variable initialization
  frequency.table = c()
  # checking the format of input data (vector? list?); converting to a list
  if(is.list(corpus) == FALSE) {
    corpus = list(corpus) 
  }
  # checking if there are any names attached to the texts
  if(is.character(names(corpus)) == FALSE) {
    # if not, some generic names will be assigned
    names(corpus) = paste("sample",1:length(corpus),sep="_")
  }

  message(paste("processing ", length(corpus), " text samples"))
  
  for(i in 1:length(corpus)) {
    message(sprintf("%7s", i), appendLF = FALSE)
    # loading the next sample (= next item) from the corpus
    current.sample = factor(corpus[[i]], levels = current.levels)
    # preparing the frequency list of the current sample
    if(relative == TRUE) {
      # either relative frequencies...
      raw.freqs = table(current.sample) * 100 / length(current.sample)
    } else {
      # ...or raw frequencies
      raw.freqs = table(current.sample)
    }
    # adjusting the frequencies to the list of features passed as an argument
    current.vector.of.freqs = raw.freqs[features]
    # create a new vector (name assgned automatically) that will contain
    # the frequencies of the current sample
    assign(paste("vector_of_freqs_", sprintf("%09d", i), sep=""), current.vector.of.freqs)
    # a short message on screen
    message("\b\b\b\b\b\b\b", appendLF = FALSE)
  }
  message("       ")
  message("combining frequencies into a table...")
  # combining frequencies into a table
  frequency.table = sapply(mget(grep("vector_of_freqs_", ls(), value=T)), rbind)
  frequency.table = t(frequency.table)  

  # un-factoring features
  features = as.character(features)
  
  # adjusting names of the samples
  rownames(frequency.table) = names(corpus)
  colnames(frequency.table) = features
  # all NA values will be set to 0
  frequency.table[which(is.na(frequency.table))] = 0
  #
  # optionally, get rid of words that have not occurred in the corpus:
  # if switched off (which makes sense when one compares two or more tables), 
  # then the non-existing words will be represented in the table,
  # and they will be filled with 0
  if(absent.sensitive == TRUE) {
    # if any of the requested words was not found in the corpus, ...
    if( length(features[colSums(frequency.table) == 0]) > 0 ) {
      # the word in question will be listed
      cat("The following words/features could not be found in the corpus:\n")
      cat(features[colSums(frequency.table) == 0], "\n")
    }
    # filtering out the variables (words) that have not occurred
    frequency.table = frequency.table[,(colSums(frequency.table) > 0)]
  }
  # complain if the frequency table is empty
  if(length(frequency.table) == 0) {
    stop("something must be wrong with the words/features you want to analyze")
  }
  # assign a class
  class(frequency.table) = "stylo.data"
  # adding some information about the current function call
  # to the final list of results
  attr(frequency.table, "call") = match.call()
# the result of the function
return(frequency.table)
}
