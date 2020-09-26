

# Function for performing Support Vector Machine classification;
# it is a wrapper for the package 'e1071'. 
# Arguments: two tables of frequencies for the training and the test sets,
# samples in rows, observations (features) in columns;
# a few tuning parameters

perform.svm = function(training.set, 
                       test.set,
                       classes.training.set = NULL,
                       classes.test.set = NULL,
                       no.of.candidates = 3,
                       tune.parameters = FALSE,
                       svm.kernel = "linear",
                       svm.degree = 3,
                       svm.coef0 = 0,
                       svm.cost = 1) {

  # Support Vector Machines classification:
  # library(e1071)
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
  
  
  # getting rid of the variables that are not represented in the training set
  check.columns = colSums(training.set)
  training.set = training.set[,(check.columns != 0)]
  test.set = test.set[,(check.columns != 0)]

  # an error is produced when variable names contain three dots: "..."
  # just in case, then:
  colnames(test.set) = gsub("\\.\\.\\.","^^^",colnames(test.set))
  colnames(training.set) = gsub("\\.\\.\\.","^^^",colnames(training.set))



  # training a model
  
  if(tune.parameters == FALSE) { # a. default parameters (faster but less accurate)
  
          model = svm(training.set, factor(classes.training.set), kernel = svm.kernel, 
                      degree = svm.degree, coef0 = svm.coef0, cost = svm.cost)
   
  } else {    # b. the parameters tuned empirically

          params = tune.svm(training.set, factor(classes.training.set),
                            gamma = 2^(-1:1), cost = 2^(1:5),
                            tunecontrol = tune.control(sampling = "boot"))
          model = svm(training.set, factor(classes.training.set), kernel = svm.kernel, 
                      degree = svm.degree, coef0 = svm.coef0, 
                      cost = params$best.parameters$cost, 
                      gamma = params$best.parameters$gamma )
                      
  }

  
  #
  # testing the model on "new" data (i.e. the test.set)
  predicted.classes = predict(model, test.set, decision.values = TRUE)

  # retrieving decision values: a composite matrix
  d.values = attr(predicted.classes, "decision.values")
  
  # get reverse values in each cell
  d.values.rev = d.values * -1
  
  # getting rid of double names in colnames
  colnames(d.values.rev) = gsub(".+/","",colnames(d.values))
  colnames(d.values) = gsub("/.+","",colnames(d.values))
  
  # combining both matrices
  xx = cbind(d.values,d.values.rev)
  
  # producing a table of "distances" (actually: decision values)
  selected.dist = c()
  
  if(length(xx[1,]) > 2) {
      for(i in unique(colnames(xx))) {
              current.col = rowMeans(xx[,colnames(xx) == i])
              selected.dist = cbind(selected.dist, current.col)
      }
  } else {
      selected.dist = xx
  }

  colnames(selected.dist) = unique(colnames(xx))


  
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
    # predicted = predicted_classes
    # expected = expected_classes
    # misclassified = cv.misclassifications
    
    attr(y, "description") = "classification results in a compact form"
    # attr(misclassified, "description") = "misclassified samples [still not working properly]"
    attr(predicted, "description") = "a vector of classes predicted by the classifier"
    attr(expected, "description") = "ground truth, or a vector of expected classes"
    attr(ranking, "description") = "predicted classes with their runner-ups"
    attr(scores, "description") = "SVM decision scores, ordered according to candidates"
    attr(raw_scores, "description") = "SVM decision scores in their original order"
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


    # adding some information about the current function call
    # to the final list of results
    results$call = match.call()
    results$name = call("perform.svm")
    
    class(results) = "stylo.results"
    
    return(results)

}

