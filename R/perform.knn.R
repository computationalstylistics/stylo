

# Function for performing k-Nearest Neighbors classification;
# it is a wrapper for the package 'class'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns
# k value: number of nearest neighbors to make a decision


perform.knn = function(training.set, test.set, 
                       classes.training.set, 
                       classes.test.set, k.value=1) {
  #kNN classification:
  # library(class)
  #
  # training_set and test_set preparation; adding class labels to both sets
  
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
actual_classes = classes.test.set
confusion.matrix = table(predicted_classes, actual_classes)
# getting rid of the classes not represented in the training set (e.g. anonymous samples)
confusion.matrix = confusion.matrix[,rownames(confusion.matrix)]


attr(classification.results, "confusion_matrix") = confusion.matrix




return(classification.results)
}


