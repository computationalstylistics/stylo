



imposters.optimize = function(reference.set,
                     classes.reference.set = NULL,
                     parameter.incr = 0.01,
                     ...) {
    
    
    
    
    # if any additional arguments are passed by a user, they will
    # be stored on the following list, and used to overwrite the defaults
    passed.arguments = list(...)

    
    # check if any classes' names were assigned to the reference set
    if(length(classes.reference.set) != length(rownames(reference.set))) {
            classes.reference.set = c(gsub("_.*", "", rownames(reference.set)))
    }
    classes.reference.set = factor(classes.reference.set)
    
    
    
    # more than one text per class
    more.than.one = names(table(classes.reference.set)[table(classes.reference.set) > 1])

    reference.set = reference.set[classes.reference.set %in% more.than.one , ]
    classes.reference.set = classes.reference.set[classes.reference.set %in% more.than.one]

    predicted.scores = c()
    expected.scores = c()

    for(i in 1:length(reference.set[,1])) {
        current.test.text = reference.set[i,]
        current.reference.set = reference.set[-c(i),]

        message(paste(rownames(reference.set)[i], " ..."))
    
        current.predicted = suppressMessages(imposters(reference.set = current.reference.set, 
                                        test = current.test.text, ...))
        current.expected = as.numeric(classes.reference.set[i] == unique(classes.reference.set))
    
        predicted.scores = c(predicted.scores, current.predicted)
        expected.scores = c(expected.scores, current.expected)
    
    }

    #rownames(scores) = rownames(reference.set)





    # a function to rescale the predicted values, given the p1 and p2 filters
    rescale = function(score, p1, p2) {
        if(score < p1) {
            new_score = p1 * score
        } else if(score >= p1 & score <= p2) {
            new_score = 0.5
        } else {
            new_score = p2 + ((1 - p2) * score)
        }
        return(new_score)
    }






    # a function to compute c_at_1 scores; this is claimed to be a good replacement 
    # for the usual AUC scores
    c_at_1 = function(predicted_scores, expected_scores) {
        N = length(predicted_scores)
        nc = 0
        nu = 0
        for(i in 1:N) {
            if(predicted_scores[i] == 0.5) {
                nu = nu + 1
            } else if( (predicted_scores[i] > 0.5) == (expected_scores[i] > 0.5)) {
                nc = nc + 1
            }
        }
        return( (1 / N) * (nc + (nu * nc / N)) )
    }





    # starting an empty matrix, to store c_at_1 scores for particular p1 and p2 values
    fit_c_at_1 = matrix(nrow = length(seq(0, 1, parameter.incr)), 
                 ncol = length(seq(0, 1, parameter.incr)))

    i = 0

    # iterating over different p1 and p2 values (via a grid search), testing how they
    # respond in relation to the expected values
    
    for(p1 in seq(0, 1, parameter.incr) ) {
    
        i = i+1
        j = 0
    
        for(p2 in seq(p1, 1, parameter.incr) ) {
            j = j+1
            predicted = sapply(as.vector(predicted.scores), rescale, p1, p2)
            current_fit = c_at_1(predicted, as.vector(expected.scores))
            fit_c_at_1[j,i] = current_fit               
        }
    }



    # it's really cool to assess the p1 and p2 values visually!
    #library(fields)
    #image.plot(fit_c_at_1, ylab = "P1", xlab = "P2")



    # which parameters' combinations yield the best attribution scores?
    # (identifying the column and the row containing the highest c_at_1 score)
    optimal_parameters = which(fit_c_at_1 == max(fit_c_at_1, na.rm = TRUE), arr.ind = TRUE)

    # get the p1 and p2 values that were responsible for the optimal results:
    # specifically, get the row and cell ID that yielded the best scores
    p1_optimized = seq(0, 1, parameter.incr)[min(optimal_parameters)]
    p2_optimized = seq(0, 1, parameter.incr)[max(optimal_parameters)]




    return(c(p1_optimized, p2_optimized))
}




