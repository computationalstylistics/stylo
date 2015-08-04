
# Eder's Delta Distance
# Function for computing Argamon's Linear Delta of a matrix of values,
# e.g. a table of word frequencies.
#
# Argument: a matrix or data table containing at least 2 rows and 2 cols 

dist.eder = function(x, scale = TRUE){

    # test if the input dataset is acceptable
    if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
        stop("cannot apply a distance measure: wrong data format!")
    }
    # then, test whether the number of rows and cols is >1
    if(length(x[1,]) < 2 | length(x[,1]) < 2) {
        stop("at least 2 cols and 2 rows are needed to compute a distance!")
    }


    # the Delta measure relies on scaled data -- if you don't have your matrix 
    # scaled already (i.e. converted to z-scores), switch this option on
    if(scale == TRUE) {
        x = scale(x)
    } 
    
    # time to apply the measure: compute Eder's Delta distance,
    # first, apply the normalization factor (a reverse rank of a word)
    n = length(x[1,])
    y.plus.e.value = t( t(x) * ((1+n:1)/n) )
    y = dist(y.plus.e.value, method = "manhattan")

    return(y)
}
