#' A function to check the character encoding in a corpus 
#' of text files and points out inconsistencies. It takes 
#' the path to a folder and returns a csv file containing
#' the most likely character encodings and some summary
#' output to the terminal.

check.encoding = function(corpus.dir = "corpus/",
                          output.file = NULL
                          ){
  
  # Constant:
  size.warning.threshold = 50
  
  # Check for 'readr' package
  test.readr = tryCatch(readr::parse_factor(c("a", "b"), letters), error = function(e) NULL)
  if(is.null(test.readr) == TRUE) {
    stop("To use this function, you have to install the library 'readr',
              e.g. by typing install.packages('readr') in the console.
              Chances are, the package 'stringi' will also be needed.")
    }

  file.list = list.files(corpus.dir)
  
  # Analyze encoding
  corpus.encodings = character()
  for(file in file.list){
    file.path = paste(corpus.dir, file, sep = "")
    file.encoding = as.character(readr::guess_encoding(file.path)[1, 1])
    corpus.encodings = append(corpus.encodings, file.encoding)
  }
  
  # Compile Results
  results = data.frame(file.list, corpus.encodings)
  colnames(results) = c("file", "encoding")
  
  # Give % of consistent results
  table = data.frame(table(results$encoding))
  sum = sum(table$Freq)
  dominant.encoding = as.character(table$Var1[table$Freq == max(table$Freq)])
  dominant.encoding.number = table$Freq[table$Freq == max(table$Freq)]
  if(length(dominant.encoding) > 1){
    encodings = paste(dominant.encoding, collapse = " and ")
    message(paste("Your files are encoded in", encodings, "."))
  } else if(dominant.encoding.number == sum){
    message(paste("All your files are encoded in", dominant.encoding))
  } else {
    message(paste(round(100*dominant.encoding.number/sum),
                "% of your files are encoded in ", 
                dominant.encoding,
                ". Files using other encoding are", sep=""))
  }
  
  # Output the first 10 inconsistent files
  if(length(dominant.encoding) > 1){
    message("Your file encodings are inconsistent!")
  } else if((sum - dominant.encoding.number) <= 10){
    write.table(results[results$encoding != dominant.encoding,],
                row.names = FALSE, 
                col.names = FALSE,
                quote = FALSE,
                sep = ": ")
  } else {
    write.table(results[results$encoding != dominant.encoding,][1:10,],
                row.names = FALSE, 
                col.names = FALSE,
                quote = FALSE,
                sep = ": ")
    message("And more...")
  }
  
  
  # Output detailed results
  if(!is.null(output.file)){
    write.csv(results, output.file)
    output.path = paste(getwd(), output.file, sep = "/")
    message(paste("Detailed results have been saved to", output.path))
  } else {
    message("To produce a full report, rerun check.encoding() and define an output csv file \n", 
    "using the option 'output.file', e.g.:\n",
    "    check.encoding(output.file = \"encoding_report.csv\")")
  }
  
  # Suggest using change.encoding()
  if(dominant.encoding[1] != "UTF-8" | dominant.encoding.number[1] != sum){
    message("Stylo works best with ASCII, but also with utf-8 encoded text files.")
    message("If you want to convert files to utf-8 encoding, try \n",
    "    iconv()              for single files or \n",
    "    change.encoding()    for entire folders")
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
  }

}
