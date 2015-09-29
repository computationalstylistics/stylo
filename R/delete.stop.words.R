
# #################################################
# This tiny function is responsible for getting rid of custom words/variables.
# It can be used as a pronoun deletion procedure, which in some languages
# improve authorship attribution (Hoover, ....); it can be also used 
# as a stop words filter.
# Applicable to either a vector containing words, or a data matrix/frame;
# the former in case of culling stop words from running text, the latter
# for culling them from tables of frequencies (then particular columns are 
# excluded). 
#
# Arguments: (i) a data frame or matrix containing samples in rows, variables
# in columns, and variables' names accessible via colnames(input.table),
# (instead of a table, vectors with running text can be used)
# (ii) a vector of words to be excluded
# #################################################

delete.stop.words =  function(input.data, stop.words = NULL) {
        
        # is input.data a matrix? a data frame?
        if(is.matrix(input.data) == TRUE | is.data.frame(input.data) == TRUE) {        
                
                # checking if any variables' names exist
                if(length(colnames(input.data)) < 1) {
                        warning(paste("stop word deletion could not be performed:",
                           "\n", "data table does not contain names\n"))

                }
                # checking if the stop words match (any) variables' names
                if(length(stop.words) > 0 && 
                length(intersect(colnames(input.data), stop.words)) == 0) {
                        warning(paste("chosen stop words were not found in the dataset;",
                        "\n", " please check the language, lower/uppercase issues, etc.\n"))                        
                }

                # extracting only those columns that match the remaining names
                culled.data = input.data[,!c(colnames(input.data) %in% stop.words)]
                
        # is input.data a vector?
        } else if(is.vector(input.data) == TRUE) {

                # checking if the stop words match (any) variables' names
                if(length(stop.words) > 0 && 
                length(intersect(input.data, stop.words)) == 0) {
                        warning(paste("chosen stop words were not found in the dataset;",
                        "\n", " please check the language, lower/uppercase issues, etc.\n"))                        
                }

                # extracting only those elements that match the remaining names
                culled.data = input.data[!(input.data %in% stop.words)]
                
        # is input.data a list?
        } else if(class(input.data) == "stylo.corpus") {

                # checking if the stop words match (any) variables' names
              #  if(length(stop.words) > 0 && 
              #  length(intersect(input.data, stop.words)) == 0) {
              #          warning(paste("chosen stop words were not found in the dataset;",
              #          "\n", " please check the language, lower/uppercase issues, etc.\n"))                        
              #  }

                # function for extracting the desired words
                extract.words = function(x, words) x[!(x %in% words)] 
                # and the procedure of extracting the words itself
                culled.data = lapply(input.data, extract.words, words = stop.words)
                class(culled.data) = "stylo.corpus"
                
        } else {
                warning(paste("chosen stop words could not be applied to the dataset:",
                          "\n"," unrecognized data structure\n"))
                culled.data = NULL
        }

return(culled.data)
}

