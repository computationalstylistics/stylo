

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
  
  
  
    names(classification.results) = rownames(test.set)
    rownames(classification.rankings) = rownames(test.set)
    rownames(classification.scores) = rownames(test.set)
    colnames(classification.rankings) = 1:no.of.candidates
    colnames(classification.scores) = 1:no.of.candidates

  
  
    # preparing a confusion table
    predicted_classes = classification.results
    expected_classes = classes.test.set

    classes_all = sort(unique(as.character(c(expected_classes, classes.training.set))))
    predicted = factor(as.character(predicted_classes), levels = classes_all)
    expected  = factor(as.character(expected_classes), levels = classes_all)
    confusion_matrix = table(expected, predicted)


    # shorten the names of the variables
    y = classification.results
    ranking = classification.rankings
    scores = classification.scores
    raw_scores = selected.dist
    features = the.features
    # predicted = predicted_classes
    # expected = expected_classes
    # misclassified = cv.misclassifications
    
    attr(y, "description") = "classification results in a compact form"
    # attr(misclassified, "description") = "misclassified samples [still not working properly]"
    attr(predicted, "description") = "a vector of classes predicted by the classifier"
    attr(expected, "description") = "ground truth, or a vector of expected classes"
    attr(ranking, "description") = "predicted classes with their runner-ups"
    attr(scores, "description") = "NSC decision scores, ordered according to candidates"
    attr(raw_scores, "description") = "NSC decision scores in their original order"
    attr(confusion_matrix, "description") = "confusion matrix for all cv folds"


    results = list()
    results$y = y
    # results$misclassified = misclassified
    results$predicted = predicted
    results$expected = expected
    results$ranking = ranking
    results$scores = scores
    results$raw_scores = raw_scores
    results$confusion_matrix = confusion_matrix
    
    
    # if 'features' is non-empty, add it as well:
    if(is.null(features) == FALSE) {
        attr(features, "description") = "the most distinctive features"
        results$features = features
    }


    # adding some information about the current function call
    # to the final list of results
    results$call = match.call()
    results$name = call("perform.nsc")
    
    class(results) = "stylo.results"
    
    return(results)

}

