

# Function for performing Naive Bayes classification;
# it is a wrapper for the package 'e1071'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns


perform.naivebayes = function(training.set, test.set,
                       classes.training.set = NULL, classes.test.set = NULL) {
  # Naive Bayes classification:
  #  library(e1071)
  #
  
  # first, sanitizing the type of input data
  if(length(dim(training.set)) != 2) {
      stop("train set error: a 2-dimensional table (matrix) is required")
  }
  # if a vector (rather than a matrix) was used as a test set, a fake row
  # will be added; actually, this will be a duplicate of the vector
  if(is.vector(test.set) == TRUE) {
      test.set = rbind(test.set, test.set)
      rownames(test.set) = c("unknown", "unknown-copy")
      # additionally, duplicating ID of the test classes (if specified)
      if(length(classes.test.set) == 1) {
          classes.test.set = c(classes.test.set, "unknown-copy")
      }
  }

  
  # training_set and test_set preparation; adding class labels to both sets
  # assigning classes, if not specified
  if(length(classes.training.set) != length(rownames(training.set))) {
          classes.training.set = c(gsub("_.*", "", rownames(training.set)))
  }

  if(length(classes.test.set) != length(rownames(test.set))) {
          classes.test.set = c(gsub("_.*", "", rownames(test.set)))
  }
  #
  classes = c(classes.training.set, classes.test.set)
  # converting strings to factors
  classes = factor(classes)
  #
  input.data = as.data.frame(rbind(training.set,test.set))
  input.data = cbind(classes, input.data)
  training.classes = c(1:length(training.set[,1]))
  #
  # an error is produced when variable names contain three dots: "..."
  # just in case, then:
  colnames(input.data) = gsub("\\.\\.\\.","^^^",colnames(input.data))
  colnames(training.set) = gsub("\\.\\.\\.","^^^",colnames(training.set))

  # training a model
  model = naiveBayes(classes ~ ., data = input.data, subset = training.classes)
  #
  # testing the model on "new" data (i.e. the test.set)
  classification.results = predict(model, input.data[,-1])
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training.set))]
  # let's see who gets linked to whom: adding names to the results
  names(classification.results) = rownames(test.set)

  
    # preparing a confusion table
    predicted_classes = classification.results
    expected_classes = classes.test.set

    classes_all = sort(unique(as.character(c(expected_classes, classes.training.set))))
    predicted = factor(as.character(predicted_classes), levels = classes_all)
    expected  = factor(as.character(expected_classes), levels = classes_all)
    confusion_matrix = table(expected, predicted)

    # shorten the names of the variables
    y = classification.results
    # predicted = predicted_classes
    # expected = expected_classes
    # misclassified = cv.misclassifications
    
    attr(y, "description") = "classification results in a compact form"
    attr(confusion_matrix, "description") = "confusion matrix for all cv folds"
    # attr(misclassified, "description") = "misclassified samples [still not working properly]"
    attr(predicted, "description") = "a vector of classes predicted by the classifier"
    attr(expected, "description") = "ground truth, or a vector of expected classes"
    

    results = list()
    results$y = y
    results$confusion_matrix = confusion_matrix
    # results$misclassified = misclassified
    results$predicted = predicted
    results$expected = expected


    # adding some information about the current function call
    # to the final list of results
    results$call = match.call()
    results$name = call("perform.naivebayes")
    
    class(results) = "stylo.results"
    
    return(results)
    
}

