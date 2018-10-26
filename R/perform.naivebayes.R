

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
  input.data = as.data.frame(rbind(training.set,test.set))
  input.data = cbind(classes, input.data)
  training.classes = c(1:length(training.set[,1]))
  #
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
  actual_classes = classes.test.set
  confusion.matrix = table(predicted_classes, actual_classes)
  # getting rid of the classes not represented in the training set (e.g. anonymous samples)
  # confusion.matrix = confusion.matrix[,rownames(confusion.matrix)]

  attr(classification.results, "confusion_matrix") = confusion.matrix


return(classification.results)
}

