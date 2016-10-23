

# Function for performing Nearest Shrunken Centroid classification;
# it is a wrapper for the package 'pamr'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns

perform.nsc = function(training.set, 
                       test.set,
                       classes.training.set = NULL,
                       classes.test.set = NULL,
                       show.features = FALSE,
                       no.of.candidates = 3) {


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

  
  # checking if the two sets are of the same size
  if(length(test.set[1,]) != length(training.set[1,])) {
          stop("training set and test set should have the same number of variables!")
  }
  
  # assigning classes, if not specified
  if(length(classes.training.set) != length(rownames(training.set))) {
          classes.training.set = c(gsub("_.*", "", rownames(training.set)))
  }  
  if(length(classes.test.set) != length(rownames(test.set))) {
          classes.test.set = c(gsub("_.*", "", rownames(test.set)))
  }
  
  #
  # training_set and test_set preparation; adding class labels to both sets
  classes = c(classes.training.set, classes.test.set)
  input.data = as.data.frame(rbind(training.set,test.set))
  training.classes = c(1:length(training.set[,1]))
  mydata = list(x = t(input.data),
              y = as.factor(classes),
              geneid = as.character(1:length(colnames(training.set))), 
              genenames = colnames(training.set)
              )
  # training a model
  model = pamr.train(mydata,sample.subset=c(1:length(classes.training.set)))
  cat("\n")
  # getting the discriminative features (if an appropriate option was chosen)
  if(show.features == TRUE) {
      the.features = pamr.listgenes(model,mydata,threshold=1,genenames=TRUE)#[,2]
  } else {
      the.features = NULL
  }
  # testing the model on "new" data (i.e. the test.set)
  classification.results = pamr.predict(model,mydata$x,threshold=1, type="posterior")
  
  selected.dist = classification.results[-c(1:length(training.set[,1])),]
  
  
    
  if(no.of.candidates > length(unique(classes.training.set))) {
          no.of.candidates = length(unique(classes.training.set))
  }


  # starting final variables
  classification.results = c()
  classification.scores = c()
  classification.rankings = c()
  
  for(h in 1:length(selected.dist[,1])) {
          ranked.c = order(selected.dist[h,],decreasing=TRUE)[1:no.of.candidates]
          current.sample = colnames(selected.dist)[ranked.c[1]]
          classification.results = c(classification.results, current.sample)
          #
          current.ranking = colnames(selected.dist)[ranked.c]
          current.scores = selected.dist[h,ranked.c]
          classification.scores = rbind(classification.scores, current.scores)
          classification.rankings = rbind(classification.rankings, current.ranking)
  }
  
  
  # preparing a confusion table
  predicted_classes = classification.results
  actual_classes = classes.test.set
  confusion.matrix = table(predicted_classes, actual_classes)
  # getting rid of the classes not represented in the training set (e.g. anonymous samples)
 # confusion.matrix = confusion.matrix[,rownames(confusion.matrix)]

  
  names(classification.results) = rownames(test.set)
  rownames(classification.rankings) = rownames(test.set)
  rownames(classification.scores) = rownames(test.set)
  colnames(classification.rankings) = 1:no.of.candidates
  colnames(classification.scores) = 1:no.of.candidates
  
  attr(classification.results, "distance.table") = selected.dist
  attr(classification.results, "rankings") = classification.rankings
  attr(classification.results, "scores") = classification.scores
  attr(classification.results, "features") = the.features
  attr(classification.results, "confusion_matrix") = confusion.matrix


return(classification.results)
}

