
# imposters.optimize()

# parameter "centroid = TRUE" (for all the classes)
# this should be switched off for SVM
# maybe the centroids should be performed by the delta classifier itself?

# class imbalance to be taken care of!



imposters = function(reference.set, 
                     test = NULL,
                     candidate.set = NULL,
                     iterations = 100,
                     features = 0.5,
                     imposters = 0.5,
                     classes.reference.set = NULL,
                     classes.candidate.set = NULL,
                     ...) {


    # if any additional arguments are passed by a user, they will
    # be stored on the following list, and used to overwrite the defaults
    passed.arguments = list(...)

    # sanitizing the parameters
    #
    # first, checking if "features" meet the condition: 0 < features < 1
    if(features > 1 | features < 0) {
        # if not, then go for the default settings
        features = 0.5
    }
    # now, the same about the proportion of "imposters": 0 < imposters < 1
    if(imposters > 1 | imposters < 0) {
        # if not, then go for the default settings
        imposters = 0.5
    }
    
    
    # sanitize the reference set: if it is a matrix etc.etc.etc
    #
    #
    #
    #
    
    
    # getting the number of features (e.g. MFWs)
    no.of.cols = length(reference.set[1,])
    
    # check if any classes' names were assigned to the reference set
    if(length(classes.reference.set) != length(rownames(reference.set))) {
            classes.reference.set = c(gsub("_.*", "", rownames(reference.set)))
    }
    classes.reference.set = factor(classes.reference.set)
    
    
    # check if any test text has been indicated; if not, try to guess it from the corpus
    if(is.null(test) == FALSE) {
        # checking if the reference sets and the sample are of the same size
        if(length(test) != no.of.cols) {
            stop("The anonymous sample and the reference set \nmust have the same number of variables!")
        }
        anon = "Anonymous"
    } else {
        # an extremely complicated way of getting the class that has 1 sample
        anon = names(table(classes.reference.set))[table(classes.reference.set) == 1]
        # checking what has been found
        if(length(anon) == 1) {
            message("")
        }
        if(length(anon) == 0) {
            message(paste("No test sample specified; taking the first available, i.e.:\n ", rownames(reference.set)[1], collapse = " "))
            # adding one additional class ("Anonymous") to the reference.set
            anon = "Anonymous"
            classes.reference.set = factor(classes.reference.set, levels =c(levels(classes.reference.set), anon))
            # changing the first text's class into "Anonymous"
            classes.reference.set[1] = anon
        }
        if(length(anon) > 1) {
            message(paste("No test sample specified; taking the following one:\t", anon[1], collapse = " "))
            anon = anon[1]
        }
        # identifying the test sample in the reference set
        test = reference.set[classes.reference.set == anon , ]
        # excluding the already-identified anonymous sample from the reference set
        reference.set = reference.set[classes.reference.set != anon , ]
        classes.reference.set = factor(classes.reference.set[classes.reference.set != anon])
    
    }
    
    
    # check if any candidate set has been indicated; if not, test them all
    # first, testing if the indicated set has >1 texts
    if(is.null(candidate.set) == FALSE & is.vector(candidate.set) == FALSE) {
        # checking if the reference sets and the sample are of the same size
        if(length(candidate.set[1,]) != no.of.cols) {
            stop("The candidate set and the reference set \nmust have the same number of variables!")
        }
        # assigning classes, if not specified
        if(length(classes.candidate.set) != length(rownames(candidate.set))) {
            classes.candidate.set = c(gsub("_.*", "", rownames(candidate.set)))
        }
        candidates = unique(classes.candidate.set)
    # if the indicated set exists but has only one text (i.e. if it is a vector)
    } else if(is.null(candidate.set) == FALSE & is.vector(candidate.set) == TRUE) {
        # checking if the reference sets and the sample are of the same size
        if(length(candidate.set) != no.of.cols) {
            stop("The candidate text and the reference set \nmust have the same number of variables!")
        }
        # assigning a class
        candidates = "Candidate"
    } else {
        candidate.set = reference.set[classes.reference.set != anon , ]
        classes.candidate.set = factor(classes.reference.set[classes.reference.set != anon])
        candidates = setdiff(classes.reference.set, anon)
        message("No candidate set specified; testing the following classes (one at a time):")
        message(paste(" ", candidates, collapse = " "))
        message(" \n")
    }
    
    
    message("Testing a given candidate against imposters...\n")
    
    final.scores = c()
    
    for(candidate in candidates) {
        
        # if only one text has been indicated to be the candidate set, don't bother
        if(is.vector(candidate.set) == TRUE) {
            current.candidate = candidate.set
        # otherwise (= in most cases) select only the texts by a current candidate
        } else {
            current.candidate = candidate.set[classes.candidate.set == candidate, ]
########
#current.candidate = colMeans(current.candidate)
        }
        # get the centroid?
        imposters.set = reference.set[classes.reference.set != candidate, ]
        # get the centroids?
        
        score = 0
        
        for(k in 1 : iterations) {
            # randomly picking the features: the percentage passed as an argument
            # is first converted into an integer
            no.of.features = round(length(imposters.set[1,]) * features)
            
            # picking a sample of the size defined by "no.of.features"; no replacement
            feature.IDs = sample(length(colnames(imposters.set)), no.of.features)
            # since some of the distance measures require that the original order 
            # of features is kept, let's sort them
            feature.IDs = sort(feature.IDs)
            
            # extracting the selection of features from the colnames of one of the sets
            feature.subset = sample(colnames(imposters.set))[feature.IDs]
            
            # randomly picking the imposters: the percentage passed as an argument
            # is first converted into an integer
            no.of.imposters = round(length(imposters.set[,1]) * imposters)
        
            # extracting the selection of imposters from the rownames 
            imposters.subset = sample(rownames(imposters.set))[1:no.of.imposters]
            
            # building new subsets given the above constrains (no. of features etc.)
            # first, shrinking the candidate set, depending on its dimensionality
            if(is.vector(current.candidate) == TRUE) {
                shrunken.candidate.set = current.candidate[feature.subset]
            } else {
                shrunken.candidate.set = current.candidate[, feature.subset]
            }
            shrunken.imposters.set = imposters.set[imposters.subset,feature.subset]
            shrunken.test = test[feature.subset]
            
            # final reshaping, for the sake of the classifier
            training.set = rbind(shrunken.candidate.set, shrunken.imposters.set)
            # THIS IS AN UGLY WORKAROUND, TO PREVENT SOME PSEUDO-FALSE POSITIVES
            # adding the candidate's class to the training set
            rownames(training.set)[1] = paste(candidate, "_test", sep = "")
            test.set = shrunken.test
            
            # THE MAIN STEP: LAUNCHING THE CLASSIFIER
            # run the classifier
            get.the.answer = perform.delta(training.set, test.set, ...)
            # getting only the first hit, from the variable y (compact results)
            get.the.answer = get.the.answer$y[1]
            
            # increasing the score when the nearest author is in the target group
            if(get.the.answer == candidate) {
                score = score + 1 / iterations
            }
            
        }
        
        # for the sake of compatibility with the original paper, reversing the scores
        #score = 1 - score
        
        # for some reason, it returns values such as -6.6613e-16; getting rid of this:
        score = round(score, 10)
        
        # message on screen
        message(paste(candidate, "\t", score))
        
        # recording the final scores 
        final.scores = c(final.scores, score)
    }
    
    names(final.scores) = candidates


return(final.scores)
}



