
# Cosine Delta Distance
# Function for computing a cosine similarity of a matrix of values,
# e.g. a table of word frequencies.
#
# Argument: a matrix or data table containing at least 2 rows and 2 cols 


dist.wurzburg = function(x){
    
    # test if the input dataset is acceptable
    if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
        stop("cannot apply a distance measure: wrong data format!")
    }
    # then, test whether the number of rows and cols is >1
    if(length(x[1,]) < 2 | length(x[,1]) < 2) {
        stop("at least 2 cols and 2 rows are needed to compute a distance!")
    }
    
    # first, z-scoring the input dataset
    x = scale(x)
    
    # checking for all-zeros columns: they spoil the final results!
    x = x[ , attr(x ,"scaled:scale") != 0]
    
    # to get Centered Cosine dist (=Pearson Correlation Coeff.), one needs 
    # to normalize the feature vectors by subtracting the vector means
    # x = t( t(x) - colMeans(x) )
    
    # this computes cosine dissimilarity; to have similarity, 1- applies
    y = 1 - as.dist( x %*% t(x) / (sqrt(rowSums(x^2) %*% t(rowSums(x^2)))) ) 
    # alternative way of approaching it:
    # crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))

    return(y)
}
