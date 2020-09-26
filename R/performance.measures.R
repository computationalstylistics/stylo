

performance.measures = function(predicted_classes,
                                expected_classes = NULL, 
                                f_beta = 1) {
    

    
    if(class(predicted_classes) == "stylo.results") {
        input_data = unclass(predicted_classes)
        predicted_classes = input_data$predicted
        expected_classes = input_data$expected
    } else {
        if(is.null(expected_classes)) {
            stop("a vector of expected classes is required! see help(performance.measures)")
        }
    }
    
    
    # sanitizing predicted and actual classes (they will be factorized later anyway)
    if(is.factor(expected_classes) == TRUE) {
        expected_classes = as.character(expected_classes)
    }
    if(is.factor(predicted_classes) == TRUE) {
        predicted_classes = as.character(predicted_classes)
    }

    

    classes_all = sort(unique(as.character(c(expected_classes, predicted_classes))))
    predicted = factor(as.character(predicted_classes), levels = classes_all)
    expected  = factor(as.character(expected_classes), levels = classes_all)
    confusion_matrix = table(expected, predicted)
    

    accuracy = sum(as.numeric(predicted == expected)) / length(expected)
    precision = diag(confusion_matrix) / colSums(confusion_matrix)
    recall = diag(confusion_matrix) / rowSums(confusion_matrix)


    # f1 measure, or the f score with beta = 1:
    # f = ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall) )

    # a generalized version:
    f = ifelse(precision + recall == 0, 0, (1 + f_beta^2) * precision * recall / ((f_beta^2 * precision) + recall) )
    
    # assigning explicit 0s to NAs and NaNs
    f[is.na(f)] = 0
    precision[is.nan(precision)] = 0
    recall[is.nan(recall)] = 0
    
    
    # binary F score or multi-class macro-averaged F score
    # f = ifelse(length(confusion_matrix[1,]) == 2, f[positive], mean(f))
    #
    # names(results) = c("accuracy", "precision", "recall", paste("F(", f_beta, ")-score", sep = ""))



    results = list()
    results$precision = precision
    results$recall = recall
    results$f = f
    results$accuracy = accuracy
    results$avg.precision = mean(precision)
    results$avg.recall = mean(recall)
    results$avg.f = mean(f)
    
    
    return(results)
}
