
# #################################################
# This function performs "culling" (cf. Hoover, 2003a, 2003b)
# Arguments: (i) a data frame or matrix containing samples in rows, variables
# in columns, and variables' names accessible via colnames(input.table)
# (ii) a percentage of texts that need to have a given word to save it
# #################################################

perform.culling <- 
function(input.table, culling.level = 0) {
        # checking if any variable names exist
        if(length(colnames(input.table)) < 1) {
                cat("culling could not be performed: no variables' names\n")
        }
        # checking for any values exceeding 0 (get TRUE/FALSE in each cell)
        nonzero.values = input.table > 0
        # counting the number of non-zero values per column;
        # first, starting a new variable
        words.after.culling = c()
        for (y in 1: length(nonzero.values[1,])) {
                words.after.culling = c(words.after.culling,
                                          (sum(as.numeric(nonzero.values[,y])) / 
                                          length(nonzero.values[,y])
                                        >= culling.level/100) )
        }

        # the culled list has no word-identification; let's change it:
        names(words.after.culling) = colnames(input.table)

        # now, getting rid of the words that have not survived culling,
        # i.e. selecting the words which got TRUE in the previous step
        words.to.be.culled = names(words.after.culling[!words.after.culling])
 
        # finally, applying the function delete.stop.words()
        culled.table = delete.stop.words(input.table, words.to.be.culled)

return(culled.table)
}


