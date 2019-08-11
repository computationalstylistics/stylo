#' A function for handling metadata, e.g. to control the coloring 
#' of graphs. By default, metadata is extracted from file names
#' in the traditional stylo fashion. Alternatively, users can
#' specify a vector of strings or factor as a grouping variable,
#' or an external csv file. The last option requires the 
#' additional specification of the actual column containing the
#' grouping variable. The file should contain one row per text
#' in the corpus, sorted in alphabetical order,  and a header 
#' line on top. 
#' There may be more elegant ways to solve this and contributions
#' are welcome...


process.metadata = function(metadata, 
                            filenames,
                            filename.column,
                            grouping.column){
  
  message("\nProcessing metadata...\n")
  
  
  # If metadata has length 1, it must be a file path, or 
  
  #####################################################
  # Option 1: No metadata, coloring from filenames
  #####################################################
  if(is.null(metadata)){
    groups = gsub("_.*","",filenames)
    message("\nAssigning plot colors according to file names...\n")
    
  } else {
    # just to make sure:
    metadata = as.character(metadata)
    
    #####################################################
    # Option 2: Metadata from R variable
    #####################################################
    if(length(metadata) > 1){
      if(length(metadata) == length(filenames)){
        groups = metadata
        message("\nAssigning plot colors according to the chosen variable...\n")
      } else {
        stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           The lengths of the metadata vector differs from the number of texts!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      }
    
    #####################################################
    # Option 3: Metadata from csv-file
    #####################################################    
    } else if(length(metadata) == 1){
      if(file.exists(metadata)){
        
        # Checking for everything that could go wrong...
        df.metadata = read.csv(metadata, header = TRUE)
        if(!is.element(grouping.column, colnames(df.metadata))){
          stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           The column you specified for grouping does not exist in the metadata file!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        } else if(!is.element(filename.column, colnames(df.metadata))){
          stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           The column you specified for filenames does not exist in the metadata file!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        } else if(length(rownames(df.metadata)) != length(filenames)){
          stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           The lengths of the metadata column differs from the number of texts!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        
        # Yeah, nothing went wrong! Now let's get the metadata:  
        } else {
          column.index = which(colnames(df.metadata) == grouping.column)
          groups = as.character(df.metadata[,column.index])
          message("\nAssigning plot colors according to the metadata file...\n")
        }
        
      } else {
        stop(metadata, " is not a valid file path!")
      }
    
      
    #####################################################
    # Worst case: the variable is completely useless...
    ##################################################### 
    } else {
      stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           Please provide useful metadata or stick to the default options!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }
  }
  
  return(groups)
  
}