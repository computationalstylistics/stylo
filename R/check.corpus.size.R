#' A function to calculate the size of a corpus folder
#' to warn users about potential consumptions of 
#' computing time and memory before processing a
#' corpus.
#' 
#' Args:
#'     corpus.dir (chr): the path to the corpus folder
#'     
#' Returns:
#'     size (num): size of the corpus in bytes
#'     

check.corpus.size = function(corpus.dir){
  
  file.list = list.files(corpus.dir)
  sizes = numeric()
  for(file in file.list){
    sizes = append(sizes, file.size(paste(corpus.dir, file, sep = "")))
  }
  size = sum(sizes)
  
  return(size)
}