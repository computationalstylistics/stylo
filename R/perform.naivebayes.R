

# Function for performing Naive Bayes classification;
# it is a wrapper for the package 'e1071'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns


perform.naivebayes = function(training.set, test.set) {
  # Naive Bayes classification:
  #  library(e1071)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
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
  classification.results = classification.results[-c(1:length(classes.training))]
  # let's see who gets linked to whom: adding names to the results
  names(classification.results) = rownames(test.set)

return(classification.results)
}

