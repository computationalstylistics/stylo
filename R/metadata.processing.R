####################
#
#
#
####################

metadata.processing = function(metadata, 
                               metadata.column,
                               names.of.texts){
  
  message("\nProcessing metadata...\n")
  
  # Get metadata from filenames or external csv file
  if(length(metadata) == 1 & is.character(metadata)){
    if(metadata == "filenames"){
      groups = gsub("_.*","",names.of.texts)
    }else if(is.character(metadata)){
      
      # At this point, we have an external metadata file
      if(file.exists(metadata)){
        df.metadata = read.csv(metadata, header = TRUE)
        if(is.null(metadata.column)){
          stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           You have to specify a column containing the grouping variable\n
           in the metadata file!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        }else if(length(rownames(df.metadata)) != length(names.of.texts)){
          stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           The lengths of the metadata column differs from the number of texts!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        } else if(!is.element(metadata.column, colnames(df.metadata))){
          stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           The column you specified does not exist in the metadata file!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
        } else {
          column.index = which(colnames(df.metadata) == metadata.column)
          message(column.index)
          groups = as.character(df.metadata[,column.index])
        }
        
        
      } else {
        stop(metadata, " is not a valid file path!")
      }
    }else{
      stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           *metadata* must be either a vector or a path to\n
           csv file of adequate length, or the string\n
           *filenames* if metadata is to be extracted from\n
           filenames automatically.
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }
    
  # Or from a vector...  
  } else {
    if(length(metadata) == length(names.of.texts)){
      groups = metadata
    } else if(!is.character(metadata) & !is.factor(metadata)){
      stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           Please provide the metadata vector as chracter or factor\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    } else {
      stop("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n
           The lengths of the metadata vector differs from the number of texts!\n
           !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }
  }
  
  return(groups)

}


