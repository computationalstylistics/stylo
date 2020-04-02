

# Function for performing k-Nearest Neighbors classification;
# it is a wrapper for the package 'class'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns
# k value: number of nearest neighbors to make a decision


perform.knn = function(training.set, test.set, 
                       classes.training.set = NULL, 
                       classes.test.set = NULL, k.value = 1) {
  #kNN classification:
  # library(class)
  #
  # training_set and test_set preparation; adding class labels to both sets
  
  
  
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

  
  # assigning classes, if not specified
  if(length(classes.training.set) != length(rownames(training.set))) {
          classes.training.set = c(gsub("_.*", "", rownames(training.set)))
  }

  if(length(classes.test.set) != length(rownames(test.set))) {
          classes.test.set = c(gsub("_.*", "", rownames(test.set)))
  }
  #
  training.set = cbind(classes.training.set,training.set)
  test.set = cbind(classes.test.set,test.set)
  #
  # classes that will be used for training the classifier (=classes of I set)
  classes = factor(training.set[,1])
  # training and classification
  classification.results = knn(training.set[,-1],test.set[,-1],classes,k=k.value)
  # cross-validation: 
  #knn.cv(training.set[,-1],classes,k=k.value,prob=T)
  # get final results
  classification.results = as.character(classification.results)
  # let's see who gets linked to whom: adding names to the results
  names(classification.results) = rownames(test.set)


    # preparing a confusion table
    predicted_classes = classification.results
    expected_classes = classes.test.set

    classes_all = sort(unique(as.character(c(expected_classes, predicted_classes))))
    predicted = factor(as.character(predicted_classes), levels = classes_all)
    expected  = factor(as.character(expected_classes), levels = classes_all)
    confusion_matrix = table(expected, predicted)

# getting rid of the classes not represented in the training set (e.g. anonymous samples)
#confusion_matrix = confusion_matrix[,rownames(confusion_matrix)]


attr(classification.results, "confusion_matrix") = confusion_matrix




return(classification.results)
}


