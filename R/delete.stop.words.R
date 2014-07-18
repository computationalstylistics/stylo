
# #################################################
# This tiny function is responsible for getting rid of custom words/variables.
# It can be used as a pronoun deletion procedure, which in some languages
# improve authorship attribution (Hoover, ....); it can be also used 
# as a stop words filter.
# Arguments: (i) a data frame or matrix containing samples in rows, variables
# in columns, and variables' names accessible via colnames(input.table)
# (ii) a vector of words to be excluded
# #################################################

delete.stop.words = function(input.table, 
                               stop.words = NULL) {
        # checking if any variable names exist
        if(length(colnames(input.table)) < 1) {
                cat("stop word deletion could not be performed\n")
        }
        # 
        if(length(stop.words) > 0 && 
                   length(intersect(colnames(input.table), stop.words)) == 0) {
                cat("chosen stop words were not found in the dataset;\n")
                cat("please check the language, lower/uppercase issues, etc.\n")

        }
        culled.table = input.table[,!c(colnames(input.table) %in% stop.words)]        
        return(culled.table)
}

