
# Min-Max Distance (aka Ruzicka Distance)
# Function for computing a cosine similarity of a matrix of values,
# e.g. a table of word frequencies.
#
# Argument: a matrix or data table containing at least 2 rows and 2 cols 


dist.minmax = function(x){
    
    # test if the input dataset is acceptable
    if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
        stop("cannot apply a distance measure: wrong data format!")
    }
    # then, test whether the number of rows and cols is >1
    if(length(x[1,]) < 2 | length(x[,1]) < 2) {
        stop("at least 2 cols and 2 rows are needed to compute a distance!")
    }
    
    # getting the size of the input table
    rows = length(x[,1])
    # starting a new matrix
    y = matrix(nrow = rows, ncol = rows)
    rownames(y) = rownames(x)
    colnames(y) = rownames(x)
    # iterating over rows and columns
    for(i in 1:rows) {
        for(j in i:rows ) {
            y[j,i] = 1 - sum(pmin(x[i,], x[j,])) / sum(pmax(x[i,], x[j,]))
        }
    }

    # converting the matrix to the class 'dist'
    y = as.dist(y)
    
    return(y)
}
