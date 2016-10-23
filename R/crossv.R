


crossv = function(training.set, 
                  test.set = NULL,
                  cv.mode = "leaveoneout",
                  cv.folds = 10,
                  classes.training.set = NULL,
                  classes.test.set = NULL,
                  classification.method = "delta",
                  ...) {


add.args = list(...)


# sanitizing the cross-validation type by resolving abbreviated names
supported.cv.modes = c("leaveoneout", "stratified")
cv.mode = supported.cv.modes[grep(cv.mode, supported.cv.modes)]
if(length(cv.mode) > 1) {
	cv.mode = cv.mode[1]
	warning("cross-validation mode ambiguous; performing leave-one-out")
}


if(length(dim(training.set)) != 2) {
	stop("train set error: a 2-dimensional table (matrix) is required")
}

#if(length(dim(test.set)) != 2) {
#	stop("test set error: a 2-dimensional table (matrix) is required")
#}



#test.set.exists = 

#if(cv.mode != "leaveoneout")



# assigning classes, if not specified
if(length(classes.training.set) != length(training.set[,1])) {
        classes.training.set = c(gsub("_.*", "", rownames(training.set)))
}

if(length(classes.test.set) != length(test.set[,1])) {
        classes.test.set = c(gsub("_.*", "", rownames(test.set)))
}

#


  message("\ncross-validation...\n")
 


  # creating an empty matrix for the final success scores
  cross.validation.results = c()
  cv.confusion.matrix = list()
  cv.misclassifications = list()

  # an table of combined frequencies of set I and II
  freq.table.both.sets.binded = rbind(training.set, test.set)
  classes.train = classes.training.set
  classes.test = classes.test.set    
  classes.both.sets = c(classes.train, classes.test)

  # for leave-one-out, overwrite the number of cv-folds, to match all samples
  if(cv.mode == "leaveoneout") {
  	  cv.folds = length(classes.train)
  }
  
  
  
  

  # beginning of k-fold cross-validation (k being the number of iterations)
  for(iterations in 1 : cv.folds) {

    train.samples = c()
    test.samples = c()
    
    
    if(cv.mode == "stratified") {
    
      # this looks for the classes that were not represented so far in I set
      for(i in unique(classes.training.set) ) {
        #
        # count the number of samples of class 'i' included originally in I set
        no.of.train.samples = sum(as.numeric(classes.training.set == i))
        # determine the class's name; sanitize the string of chars
        class.name = paste("\\b",i,"\\b",sep="")
        # in both sets, identify the positions of current class' samples 
        pinpoint.samples = grep(class.name, classes.both.sets)
        # sanity check, just in case
        if(length(pinpoint.samples) > no.of.train.samples) {
                # select randomly N items from the pinpoited positions
                pick.train = sample(pinpoint.samples, no.of.train.samples)
                # identify the remaining ones: future test set samples
                pick.test = setdiff(pinpoint.samples, pick.train)
                # pick the names at the positions identified above
                train.samples = c(train.samples, pick.train)
        }
      }
      
    } else {
    	# whean leave-one-out was chosen, get rid with the i-th train sample
    	train.samples = (1:length(classes.training.set))[-c(iterations)]
    }



    # establishing the training set for the current cv fold:
    cv.train = freq.table.both.sets.binded[train.samples,]
    cv.classes.train = classes.both.sets[train.samples]

    # establishing the test set for the current cv fold
    cv.test = freq.table.both.sets.binded[-c(train.samples),]
    cv.classes.test = classes.both.sets[-c(train.samples)]



    if(tolower(classification.method) == "delta") {
      classification.results = perform.delta(cv.train, cv.test, 
                        cv.classes.train, cv.classes.test, ...)
    }
    if(tolower(classification.method) == "knn") {
      classification.results = perform.knn(cv.train, cv.test, 
                        cv.classes.train, cv.classes.test, ...)
    }
    if(tolower(classification.method) == "svm") {
      classification.results = perform.svm(cv.train, cv.test, 
                        cv.classes.train, cv.classes.test, ...)
    }
    if(tolower(classification.method) == "nsc") {
      classification.results = perform.nsc(cv.train, cv.test, 
                        cv.classes.train, cv.classes.test, ...)
    }
    if(tolower(classification.method) == "naivebayes") {
      classification.results = perform.naivebayes(cv.train, cv.test, 
                        cv.classes.train, cv.classes.test, ...)
    }

  
    # returns the number of correct attributions
    no.of.correct.attrib = sum(as.numeric(cv.classes.test == 
                                   classification.results))
    # getting the max. number of samples that couold be guessed
    perfect.guessing.cv = sum(as.numeric(cv.classes.test %in% cv.classes.train))
    # percentage of correct attributions
    success.rate.cv = no.of.correct.attrib / perfect.guessing.cv * 100
    
    misclassifications = classification.results[cv.classes.test != classification.results]
    confusion.matrix = attr(classification.results, "confusion_matrix")
    
    # combining results for k folds
    cross.validation.results = c(cross.validation.results, success.rate.cv)
    cv.confusion.matrix[[iterations]] = confusion.matrix
    cv.misclassifications[[iterations]] = misclassifications
    
    
  }

  attr(cross.validation.results, "confusion_matrix") = cv.confusion.matrix
  attr(cross.validation.results, "misclassifications") = cv.misclassifications

  
  class(cross.validation.results) = "stylo.data"
  return(cross.validation.results)
}

