

performance.measures = function(predicted_classes, actual_classes, beta = 1, positive = "1", drop_test_classes = TRUE) {

    predicted = factor(as.character(predicted_classes), levels = unique(as.character(actual_classes)))
    expected  = as.factor(actual_classes)
    confusion_matrix = as.matrix(table(actual_classes, predicted_classes))

    
    # getting rid of classes that are not represented in the training set
    # by dropping the respecive columns (usually by anonymous authors)
    if(drop_test_classes == TRUE) {
        confusion_matrix = confusion_matrix[unique(predicted_classes),]
    }
    
    
    accuracy = sum(as.numeric(predicted == expected)) / length(expected)
#    accuracy = diag(confusion_matrix) / sum(confusion_matrix)
    precision = diag(confusion_matrix) / colSums(confusion_matrix)
    recall = diag(confusion_matrix) / rowSums(confusion_matrix)

    #  recall = tryCatch (diag(confusion_matrix) / rowSums(confusion_matrix), warning = function(e) warning("stylo warning: The number of the predicted classes seems to be bigger\n  than the number of the expected classes; this might happen when 'anonymous'\n  texts are represented in the confusion matrix. Please try switching on\n  the option 'performance.measures(..., drop_test_classes = TRUE)'."))

    
    
    # f1 measure, or the f score with beta = 1:
    #f = ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall) )

    # a generalized version:
    f = ifelse(precision + recall == 0, 0, (1 + beta^2) * precision * recall / ((beta^2 * precision) + recall) )
    
    # assigning explicit 0s to NAs
    f[is.na(f)] = 0

    # binary F score or multi-class macro-averaged F score
    f = ifelse(nlevels(actual_classes) == 2, f[positive], mean(f))
    precision = mean(precision)
    recall = mean(recall)
    
    results = c(accuracy, precision, recall, f)
    names(results) = c("accuracy", "precision", "recall", paste("F(", beta, ")score", sep = ""))
    
    return(results)
}




