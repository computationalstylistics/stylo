
# #################################################
# Function for loading text files 
# Usage: load.corpus(files=c("first.txt","second.txt",...))
# Required argument: vector containing file names
# Optional argument: a directory containing the corpus
# #################################################

# ATTENTION: this function is not resistant to possible mismatch
# between the vector of filenames and actual existing files;
# btw: implementation will be very easy using %in%

load.corpus <-
function(files, corpus.dir = "") {
  # first of all, retrieve the current path name
  original.path = getwd()
  # checking if the specified directory exists
  if(is.character(corpus.dir) == TRUE & nchar(corpus.dir) > 0) {
    # checking if the desired file exists and if it is a directory
    if(file.exists(corpus.dir) == TRUE & file.info(corpus.dir)[2] == TRUE) {
    # if yes, then set the new working directory
    setwd(corpus.dir)
    } else {
    # otherwise, stop the script
    stop("there is no directory ", getwd(), "/", corpus.dir)
    }
  } else {
    # if the argument was empty, then relax
    cat("using current directory...\n")
  }
  # variable initialization
  loaded.corpus = list()
  # uploading all files listed in the vector "files"
  for (file in files) {
    cat(paste("Loading ", file, "\t", "...", "\n", sep=""))
    # loading the next file from the list "corpus.filenames"
    current.file = tolower(scan(file,what="char",sep="\n", quiet=T))
    loaded.corpus[[file]] = current.file
  }
  setwd(original.path)
  
  # assigning a class
  class(loaded.corpus) = "stylo.corpus"
  # adding some information about the current function call
  # to the final list of results
  attr(loaded.corpus, "call") = match.call()

# returning the value
return(loaded.corpus)
}
