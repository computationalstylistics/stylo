#' A function to change the character encoding in a corpus 
#' of text files.
#' 
#' -> A wrapper around iconv() to process entire folders

change.encoding = function(corpus.dir = "corpus/",
                           from,
                           to = "utf-8",
                           keep.original = TRUE,
                           output.dir = NULL){
  
  # Constant:
  size.warning.threshold = 50
  
  # Warn about overwriting original coprus folder
  if(keep.original == FALSE){
    answer = readline(prompt = "Do you really want to overwrtie the original corpus files (y/n)?")
    if(answer == "n"){
      stop("Aborting conversion.")
    }
  }
  
  # Warn if corpus is very large.
  corpus.size = check.corpus.size(corpus.dir)/(10^6)
  corpus.size.output = format(corpus.size, scientific = FALSE, digits = 0)
  if(corpus.size > size.warning.threshold){
    message(paste("WARNING: Your corpus is", 
                  corpus.size.output, 
                  "MB large. Producing a version with a new encoding \n", 
                  "will use another", corpus.size.output,
                  "MB of memory on your hard drive!"
    ))
    answer = readline(prompt = "Do you really want procede (y/n)?")
    if(answer == "n"){
      stop("Aborting conversion.")
    }
  }
  
  # Create an output folder with a temporary name
  if(is.null(output.dir)){
    dir.create(".locvstemporalisovtpvtis")
    output.dir = ".locvstemporalisovtpvtis/"
    keep.corpus.folder = TRUE
  } else {
    keep.corpus.folder = FALSE
  }
  
  # Convert files
  file.list = list.files(corpus.dir)
  for(file in file.list){
    in.path = paste(corpus.dir, file, sep = "")
    out.path = paste(output.dir, file, sep = "")
    text = paste(readLines(in.path, warn = FALSE), collapse=" ")
    text = iconv(text, from = from, to = to)
    write(text, out.path)
    message(paste("Converting", file, "to", to))
  }
  
  # Rename output folder to 'corpus'
  if(keep.corpus.folder == TRUE){
    file.rename(corpus.dir, paste(substr(corpus.dir, 1, nchar(corpus.dir)-1), "old_encoding/", sep = "_"))
    file.rename(".locvstemporalisovtpvtis", "corpus")
  }
  
}