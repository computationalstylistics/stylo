
# Eder's Simple Distance
# Function for computing Eder's Simple Distance of a matrix of values,
# e.g. a table of word frequencies.
#
# Argument: a matrix or data table containing at least 2 rows and 2 cols 

dist.simple = function(x){

    # test if the input dataset is acceptable
    if(is.matrix(x) == FALSE & is.data.frame(x) == FALSE) {
        stop("cannot apply a distance measure: wrong data format!")
    }
    # then, test whether the number of rows and cols is >1
    if(length(x[1,]) < 2 | length(x[,1]) < 2) {
        stop("at least 2 cols and 2 rows are needed to compute a distance!")
    }


    # time to apply the measure: compute Eder's Simple distance,
    # simply by applying square root to the frequencies
    y = dist(sqrt(x), method = "manhattan")

    return(y)
}
