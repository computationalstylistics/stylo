

performance.measures = function(predicted_classes, 
                                actual_classes, 
                                f_beta = 1, 
                                drop_test_classes = TRUE ) {
    
    
    
    
    predicted = factor(as.character(predicted_classes), levels = unique(as.character(actual_classes)))
    expected  = as.factor(actual_classes)
    confusion_matrix = as.matrix(table(actual_classes, predicted_classes))

    
    # getting rid of classes that are not represented in the training set
    # by dropping the respecive columns (usually, texts by anonymous authors)
    if(drop_test_classes == TRUE) {
        confusion_matrix = confusion_matrix[colnames(confusion_matrix),]
    }
    
    
    accuracy = sum(as.numeric(predicted == expected)) / length(expected)
    precision = diag(confusion_matrix) / colSums(confusion_matrix)
    recall = diag(confusion_matrix) / rowSums(confusion_matrix)


       
    # f1 measure, or the f score with beta = 1:
    #f = ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall) )

    # a generalized version:
    f = ifelse(precision + recall == 0, 0, (1 + f_beta^2) * precision * recall / ((f_beta^2 * precision) + recall) )
    
    # assigning explicit 0s to NAs
    f[is.na(f)] = 0
    
    
    
    
    # binary F score or multi-class macro-averaged F score
#    f = ifelse(length(confusion_matrix[1,]) == 2, f[positive], mean(f))
    #
    
#    names(results) = c("accuracy", "precision", "recall", paste("F(", f_beta, ")-score", sep = ""))




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
