


crossv = function(training.set, 
                  test.set = NULL,
                  cv.mode = "leaveoneout",
                  cv.folds = 10,
                  classes.training.set = NULL,
                  classes.test.set = NULL,
                  classification.method = "delta",
                  ...) {
    #
    
    
    
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
	
	
	#if(length(dim(test.set)) != 2) 
	#	stop("test set error: a 2-dimensional table (matrix) is required")
	#}
	
	
	
	# assigning classes, if not specified
	if(length(classes.training.set) != length(training.set[,1])) {
        classes.training.set = c(gsub("_.*", "", rownames(training.set)))
    }
    
    if(length(classes.test.set) != length(test.set[,1])) {
        classes.test.set = c(gsub("_.*", "", rownames(test.set)))
    }
    
    
    
    message("\ncross-validation...\n")
 

    # creating an empty matrix for the final success scores
    cross.validation.results = c()
    cv.confusion.matrix = list()
    cv.misclassifications = list()
    
    predicted_classes = c()
    expected_classes = c()
    
    
    
    # an table of combined frequencies of set I and II
    freq.table.both.sets.bound = rbind(training.set, test.set)
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
            
            # this looks for the classes that were not represented so far in the training set
            for(i in unique(classes.training.set) ) {
                #
                # count the number of samples of class 'i' included originally in the training set
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
        cv.train = freq.table.both.sets.bound[train.samples,]
        cv.classes.train = classes.both.sets[train.samples]
        
        # establishing the test set for the current cv fold
        cv.test = freq.table.both.sets.bound[-c(train.samples),]
        cv.classes.test = classes.both.sets[-c(train.samples)]
        
        # additionally, sanitizing the test set
        # whenever it contains but 1 sample, another 'fake' sample will be added
        if(cv.mode == "leaveoneout") {
            cv.test = rbind(cv.test, cv.test)
            sample.name = rownames(freq.table.both.sets.bound)[-c(train.samples)]
            rownames(cv.test) = c(sample.name, "fake_row")
            cv.classes.test = c(cv.classes.test, cv.classes.test)
            fake.row.test.set = TRUE
        }
        
        # now, performing classification:
        if(tolower(classification.method) == "delta") {
            classification.results = perform.delta(cv.train, cv.test, 
                                     cv.classes.train, cv.classes.test, ...)$y
        }
        if(tolower(classification.method) == "knn") {
            classification.results = perform.knn(cv.train, cv.test, 
                                     cv.classes.train, cv.classes.test, ...)$y
        }
        if(tolower(classification.method) == "svm") {
            classification.results = perform.svm(cv.train, cv.test, 
                                     cv.classes.train, cv.classes.test, ...)$y
        }
        if(tolower(classification.method) == "nsc") {
            classification.results = perform.nsc(cv.train, cv.test, 
                                     cv.classes.train, cv.classes.test, ...)$y
        }
        if(tolower(classification.method) == "naivebayes") {
            classification.results = perform.naivebayes(cv.train, cv.test, 
                                     cv.classes.train, cv.classes.test, ...)$y
        }
        

        # now, getting rid of the overfluous fake sample for 'leaveoneout'
        if(cv.mode == "leaveoneout") {
            classification.results = classification.results[1]
            cv.classes.test = cv.classes.test[1]
        }
        
        
        # returns the number of correct attributions
        no.of.correct.attrib = sum(as.numeric(cv.classes.test == classification.results))
        # getting the max. number of samples that couold be guessed
        perfect.guessing.cv = sum(as.numeric(cv.classes.test %in% cv.classes.train))
        # percentage of correct attributions
        success.rate.cv = no.of.correct.attrib / perfect.guessing.cv
        misclassifications = classification.results[cv.classes.test != classification.results]
        if(length(misclassifications) == 0) {
            misclassifications = NULL
        }
        
        
        # combining results for k folds
        predicted_classes = c(predicted_classes, classification.results)
        expected_classes = c(expected_classes, cv.classes.test)
        
        cross.validation.results = c(cross.validation.results, success.rate.cv)
        cv.misclassifications[[iterations]] = misclassifications
    
    }
    
    
   #### cross.validation.results
    
    
    
    classes_all = sort(unique(as.character(c(expected_classes, predicted_classes))))
    predicted = factor(as.character(predicted_classes), levels = classes_all)
    expected  = factor(as.character(expected_classes), levels = classes_all)
    confusion_matrix = table(expected, predicted)

    

    # shorten the names of the variables
    y = cross.validation.results
    # predicted = predicted_classes
    # expected = expected_classes
    misclassified = cv.misclassifications
    
    attr(y, "description") = "classification results in a compact form"
    attr(confusion_matrix, "description") = "confusion matrix for all cv folds"
    attr(misclassified, "description") = "misclassified samples [still to be elaborated]"
    attr(predicted, "description") = "a vector of classes predicted by the classifier"
    attr(expected, "description") = "ground truth, or a vector of expected classes"
    

    results = list()
    results$y = y
    results$confusion_matrix = confusion_matrix
    results$misclassified = misclassified
    results$predicted = predicted
    results$expected = expected


    # adding some information about the current function call
    # to the final list of results
    results$call = match.call()
    results$name = call("crossv")
    
    class(results) = "stylo.results"
    
    return(results)
    
}

