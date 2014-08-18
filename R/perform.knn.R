

# Function for performing k-Nearest Neighbors classification;
# it is a wrapper for the package 'class'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns
# k value: number of nearest neighbors to make a decision


perform.knn = function(training.set, test.set, k.value=1) {
  #kNN classification:
  # library(class)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  training.set = cbind(classes.training,training.set)
  test.set = cbind(classes.test,test.set)
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

return(classification.results)
}


