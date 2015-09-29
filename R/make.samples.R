
# #################################################
# Function for sampling
# Usage: ...
# Required argument: ...
# Optional arguments: ....
# #################################################

make.samples <-
function(tokenized.text,
            sample.size = 10000,
            sampling = "no.sampling",
            sample.overlap = 0,
            number.of.samples = 1,
            sampling.with.replacement = FALSE){


  # checking the format of input data (vector? list?); converting to a list
  if(is.list(tokenized.text) == FALSE) {
    tokenized.text = list(tokenized.text) 
  }
  # checking if there are any names attached to the texts
  if(is.character(names(tokenized.text)) == FALSE) {
    # if not, some generic names will be assigned
    names(tokenized.text) = paste("paste",1:length(tokenized.text),sep="_")
  }
  # starting an empty list
  corpus.cut.into.samples = list()
  # iterating over subsequent texts of the input corpus
  for(i in 1:length(tokenized.text)) {
    # retrieving an appropriate text from the whole corpus (if applicable)
    current.text = tokenized.text[[i]]
    # sanity check for text length: abort if the current text is extremely
    # short or at least shorter than the specified sample size
    if (length(current.text) < 10 || 
        (sampling == "normal.sampling" && length(current.text) < sample.size) || 
        (sampling == "random.sampling" && length(current.text) < sample.size)) {
      cat("\n\n",head(current.text,100), "...\t", "This text is too short!", 
          "\n\n")
      stop("Corpus error...")
    }
    #
    if(sample.size < sample.overlap) {
            cat("------------------------------------------------\n")
            cat("Sample overlap bigger that sample size?! NO WAY!\n")
            cat("performing no sample overlap...\n")
            cat("------------------------------------------------\n")
            sample.overlap = 0
    }
    # at this point, each text in the corpus has been tokenized
    # into an array of tokens which we can divide into samples
    samples.from.text = list()
    if (sampling == "normal.sampling"){
      # initialize variables to sample the text
      text.length = length(current.text)
      number.of.samples = floor((text.length-sample.overlap)/(sample.size-sample.overlap))
      cat(names(tokenized.text)[i],"\n")
      cat(paste("\t", "- text length (in words): ", text.length, "\n", sep=""))
      cat(paste("\t", "- nr. of samples: ", number.of.samples, "\n", sep=""))
      cat(paste("\t", "- nr. of words dropped at the end of the text: ", 
                text.length-(number.of.samples*(sample.size-sample.overlap)), "\n", sep=""))
      # iterate over the samples:
      current.start.index = 1
      for(sample.index in 1:number.of.samples) {
        current.sample = current.text[current.start.index:(current.start.index+sample.size-1)]
        # flush current sample:
        samples.from.text[[sample.index]] = current.sample
        # assign a new id to current sample
        id = paste(names(tokenized.text)[i],"_",sample.index,sep="")
        names(samples.from.text)[sample.index] = id
        # increment index for next iteration
        current.start.index = current.start.index + sample.size - sample.overlap
        current.sample = c()
      }
    } else if(sampling == "random.sampling"){
      # if random sampling was chosen, the text will be randomized and a sample 
      # of a given length will be excerpted;
      # initialize variables to sample the text
      text.length = length(current.text)
      cat(names(tokenized.text)[i],"\n")
      cat(paste("\t", "- text length (in words): ", text.length, "\n", sep=""))
      cat(paste("\t", "- nr. of random samples: ", number.of.samples, "\n", sep=""))
      cat(paste("\t", "- sample length: ", sample.size, "\n", sep=""))
      # iterate over the samples:
      for(sample.index in 1:number.of.samples) {
        current.sample = head(sample(current.text, replace = sampling.with.replacement), sample.size)
        # flush current sample:
        samples.from.text[[sample.index]] = current.sample
        # assign a new id to current sample
        id = paste(names(tokenized.text)[i],"_",sample.index,sep="")
        names(samples.from.text)[sample.index] = id
        # increment index for next iteration
        current.sample = c()
      }
    } else if (sampling == "no.sampling"){
      # entire texts will be used as a sample (regardless of its length)
      current.sample = tokenized.text
      samples.from.text[[1]] = current.sample
      # inheriting the sample's name
      names(samples.from.text) = names(tokenized.text)[i]
    }
    #
    # estimating the number of samples already appended to the "new" corpus
    n = length(corpus.cut.into.samples)
    # appending newly created samples to the "new" corpus
    for(s in 1:length(samples.from.text)) {
      corpus.cut.into.samples[[n+s]] = samples.from.text[[s]]
      names(corpus.cut.into.samples)[n+s] = names(samples.from.text)[s]
    }
  }
class(corpus.cut.into.samples) = "stylo.corpus"
return(corpus.cut.into.samples)
}
