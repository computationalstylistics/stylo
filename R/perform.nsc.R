

# Function for performing Nearest Shrunken Centroid classification;
# it is a wrapper for the package 'pamr'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns

perform.nsc = function(training.set, test.set) {
# Nearest Shrunken Centroid classification:
  #  library(pamr)
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes.training = gsub("_.*","",rownames(training.set))
  classes.test = gsub("_.*","",rownames(test.set))
  classes = c(classes.training, classes.test)
  input.data = as.data.frame(rbind(training.set,test.set))
  training.classes = c(1:length(training.set[,1]))
  mydata=list(x = t(input.data),
              y = as.factor(classes),
              geneid = as.character(1:length(colnames(training.set))), 
              genenames = colnames(training.set)
              )
  # training a model
  model = pamr.train(mydata,sample.subset=c(1:length(classes.training)))
# getting the most discriminative features
#  the.features = pamr.listgenes(model,mydata,threshold=5,genenames=TRUE)[,2]
  # testing the model on "new" data (i.e. the test.set)
  classification.results = pamr.predict(model,mydata$x,threshold=1)
  classification.results = as.character(classification.results)
  classification.results = classification.results[-c(1:length(classes.training))]
  # let's see who gets linked to whom: adding names to the results
  names(classification.results) = rownames(test.set)

return(classification.results)
}

