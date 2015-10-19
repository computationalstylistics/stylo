

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

  # getting the number of features (e.g. MFWs)
  no.of.cols = length(training.set[1,])
    
  # checking if the two sets are of the same size
  if(length(test.set[1,]) != no.of.cols) {
          stop("training set and test set should the same number of variables!")
  }
  
  # assigning classes, if not specified
  if(length(classes.training.set) != length(rownames(training.set))) {
          classes.training.set = c(gsub("_.*", "", rownames(training.set)))
  }  
  if(length(classes.test.set) != length(rownames(test.set))) {
          classes.test.set = c(gsub("_.*", "", rownames(test.set)))
  }
  
  #


#  classes.training.set = gsub("_.*","",rownames(training.set))
#  classes.test.set = gsub("_.*","",rownames(test.set))
  classes = c(classes.training.set, classes.test.set)
  input.data = as.data.frame(rbind(training.set,test.set))
  input.data = cbind(classes, input.data)
  training.classes = c(1:length(training.set[,1]))
  
  # an error is produced when variable names contain three dots: "..."
  # just in case, then:
  colnames(input.data) = gsub("\\.\\.\\.","^^^",colnames(input.data))
  colnames(training.set) = gsub("\\.\\.\\.","^^^",colnames(training.set))
  #
  # training a model
  # a. default/custom parameters (faster but less accurate)
  if(tune.parameters == FALSE) {
          model = svm(classes ~ ., data = input.data, subset = training.classes, 
                 kernel = svm.kernel, degree = svm.degree, coef0 = svm.coef0, 
                 cost = svm.cost)
  # b. the parameters tuned empirically
  } else {
          tuning.data = cbind(classes.training.set, as.data.frame(training.set))
          colnames(tuning.data)[1] = "classes"
          params = tune.svm(classes ~ ., data = tuning.data,
                            gamma = 2^(-1:1), cost = 2^(2:4),
                            tunecontrol = tune.control(sampling = "boot"))
          #
          model = svm(classes ~ ., data = input.data, subset = training.classes, 
                 kernel = svm.kernel, degree = svm.degree, coef0 = svm.coef0, 
                 cost=params$best.parameter[[2]], 
                 gamma=params$best.parameter[[1]])
  }

  
  #
  # testing the model on "new" data (i.e. the test.set)
  classification.results = predict(model, input.data[,-1], decision.values=T)
  
  # retrieving decision values: a composite matrix
  d.values = attr(classification.results, "decision.values")
  
  # get reverse values in each cell
  d.values.rev = d.values * -1
  
  # getting rid of double names in colnames
  colnames(d.values.rev) = gsub(".+/","",colnames(d.values))
  colnames(d.values) = gsub("/.+","",colnames(d.values))
  
  # combining both matrices
  xx = cbind(d.values,d.values.rev)

  # getting rid of the trainig samples
  xx = xx[-c(1:length(training.set[,1])),]
  
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
  
  attr(classification.results, "distance.table") = selected.dist
  attr(classification.results, "rankings") = classification.rankings
  attr(classification.results, "scores") = classification.scores
  
return(classification.results)
}

