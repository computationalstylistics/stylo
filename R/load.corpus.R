
# #################################################
# Function for loading text files 
# Usage: load.corpus(files = c("first.txt", "second.txt", ...))
# Required argument: vector containing file names
# Optional argument: a directory containing the corpus
# #################################################


load.corpus = function(files = "all", corpus.dir = "", encoding = "UTF-8") {

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
    message("using current directory...\n")
  }
  # now, checking which files were requested; usually, the user is 
  # expected to specify a vector with samples' names
  if(length(files) == 1 & files[1] == "all") {
  	  files = list.files()
  }
  # variable initialization
  loaded.corpus = list()
  # uploading all files listed in the vector "files"
  for (file in files) {
    if(file.exists(file) == FALSE) {
      message("!\n")
      message("\"", file, "\"? no such a file -- check your directory!\n")
    } else {
      message("loading ", file, "\t", "...")
      # loading the next file from the list "corpus.filenames";
      # if an error occurred, ignore it and send a message on the screen
      current.file = tryCatch(scan(file, what = "char", encoding = encoding,
                                   sep = "\n", quiet = TRUE),
                              error = function(e) NULL)
      # if successful, append the scanned file into the corpus,
      # otherwise send a message
        if(length(current.file) > 0) {
          loaded.corpus[[file]] = current.file
        } else {
          message("!\n")
          message("the file ", file, " could not be loaded for an unknown reason\n")
        }
    }
  }
  setwd(original.path)
  
  # assigning a class, if at least one text was successfully loaded
  if( length(loaded.corpus) > 0) {
    class(loaded.corpus) = "stylo.corpus"
    # adding some information about the current function call
    # to the final list of results
    attr(loaded.corpus, "call") = match.call()
  } else {
    loaded.corpus = NULL
  }

# returning the value
return(loaded.corpus)
}
