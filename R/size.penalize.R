
size.penalize = function(training.frequencies = NULL, 
                         test.frequencies = NULL,
                         training.corpus = NULL, 
                         test.corpus = NULL,
                         mfw = c(100, 200, 500),
                         features = NULL, 
                         path = NULL, 
                         corpus.dir = "corpus",
                         sample.size.coverage = seq(100, 10000, 100),
                         sample.with.replacement = FALSE,
                         iterations = 100,
                         classification.method = "delta",
                         list.cutoff = 1000,
                         ...) {
    #
    
    
    # fist, capturing any additional parameters passed by the user
    add.args = list(...)
    
    
    
    
#    ###############################################################
#    
#    # testing if multicore environment ('doMC', 'parallel') can be used
#    test_doMC = tryCatch(doMC::registerDoMC(cores = parallel::detectCores()), 
#                         error = function(e) NULL)
#    # switching to either parallel, or serial mode, depending on the above test
#    if(length(test_doMC) > 0) {
#        parallel_mode = TRUE
#        doMC::registerDoMC(cores = parallel::detectCores())
#    } else {
#        parallel_mode = FALSE
#    }
#   
#    ###############################################################
    
    
    
    
##### temporary!! ######

# this needs to be replaced with "..."
# and, in several functions language needs to be replaced with corpus.lang:
# txt.to.words.ext, parse.corpus, load.corpus.and.parse, stylo, classify (x2), oppose (x3), rolling.classify (x3)
# the same applies to man pages!!!



#training.corpus = c("ABronte_Agnes", "ABronte_Tenant")
##### temporary!! ######    




# if(training.frequencies == NULL)

    input.texts = load.corpus.and.parse(files = "all", corpus.dir = corpus.dir, ...)
    wordlist = make.frequency.list(input.texts, head = list.cutoff)
    doc.term.matrix = make.table.of.frequencies(corpus = input.texts, features = wordlist)
    
# else:     doc.term.matrix = training.frequencies

# if(training.corpus == NULL) needs to be re-thought!
    if(is.null(training.corpus) == TRUE) {
        test.texts = rownames(doc.term.matrix)
    } else {
        test.texts = training.corpus ## or names(training.corpus) !!!!!
    }
##### temporary!! ######    
    


    message("")
    message("Testing ", length(sample.size.coverage), " sample sizes raging from ", 
            min(sample.size.coverage), " to ", max(sample.size.coverage), 
            " words (or other elements),\n", 
            "as defined by the argument 'sample.size.coverage'; for every single sample size ", 
            iterations, " random samples are drawn, as defined by the argument 'iterations'.")



    # function (iterator) to get random samples from a given input text 
    get.vector.of.freqs = function(tokenized.text) {
        current.sample = sample(tokenized.text, size = current.sample.size, 
                                replace = sample.with.replacement)
        relative.frequencies = table(current.sample) / length(current.sample) * 100
        vector.of.freqs = relative.frequencies[wordlist]
        names(vector.of.freqs) = wordlist
        vector.of.freqs[which(is.na(vector.of.freqs))] = 0
        return(vector.of.freqs)
    }
    
    
    
    # function (iterator) to perform the classification stage
    perform.classification = function(no.of.features) {
        if(classification.method == "delta") {
            predicted_classes = perform.delta(train.table[,1:no.of.features], 
                                    test.table[,1:no.of.features], z.scores.both.sets = FALSE, ...)
        }
        if(classification.method == "svm") {
            predicted_classes = perform.svm(train.table[,1:no.of.features], 
                                    test.table[,1:no.of.features], ...)
        }
        if(classification.method == "nsc") {
            predicted_classes = perform.nsc(train.table[,1:no.of.features], 
                                    test.table[,1:no.of.features], ...)
        }
#### this class matching seems a bit weird!
        actual_classes = gsub("_.*" ,"", names(predicted_classes))
        predicted_classes = as.character(predicted_classes)
        training_classes = gsub("_.*" ,"", rownames(train.table))
        
        predicted = factor(predicted_classes, levels = unique(as.character(training_classes)))
        expected  = as.factor(actual_classes)
        confusion_matrix = as.matrix(table(expected, predicted))
        
        results = confusion_matrix
        accuracy = sum(actual_classes == predicted_classes)
        attr(results, "accuracy") = accuracy
        return(results)
    }
    
    
    # function to compute Simpson's index of diversity
    get.dispersion = function(x) {
        l = sum(x * (x-1)) / (sum(x) * (sum(x) -1))
        #l = ( 4 * sum(x) * (sum(x)-1) * (sum(x)-2) * sum((x/sum(x))^3) + 
             # 2 * sum(x) * (sum(x)-1) * sum((x/sum(x))^2) - 2 * sum(x) * (sum(x-1)) * 
             # (2*sum(x)-3) * (sum((x/sum(x))^2)^2) ) / ( (sum(x) * (sum(x)-1))^2 )
        return(l)
    }
    
    
    # variance
    # Ds = ( sum( (x / sum(x) )^3 ) - (sum( (x / sum(x) )^2 )^2 ) ) / (sum(x)/4)
    # a better version:
    # Ds = ( 4 * sum(x) * (sum(x)-1) * (sum(x)-2) * sum((x/sum(x))^3) + 2 * sum(x) * (sum(x)-1) * sum((x/sum(x))^2) - 2 * sum(x) * (sum(x-1)) * (2*sum(x)-3) * (sum((x/sum(x))^2)^2) ) / ( (sum(x) * (sum(x)-1))^2 )
    
    
    

    
    # starting empty variables, to collect final results
    joint.accuracy.scores = list()
    joint.diversity.scores = list()
    joint.confusion.matrices = list()
    iteration.counter = 0    
    
    
    # an outer loop, to iterate over text samples
    for(test.text in test.texts) {
        
        # showing the currently processed sample on screen
        message("")
        message(test.text)
        # setting a counter of iterations (= processed samples)
        iteration.counter = iteration.counter +1

        train.table = doc.term.matrix[rownames(doc.term.matrix) != test.text,]   

    
##### MAKE SURE THE TEXT EXISTS in the input.texts object!
##### if loaded from an external object, it has to be taken into account!
        get.test.text = input.texts[[grep(test.text, names(input.texts))]]
        
        
        
        
        # starting some new (empty) variables
        accuracy_all = c()
        diversity_all = c()
        confusion_matrices_all = list()
        counter.alt = 0

        
        # now, iterating over specified range of sample sizes to asses
        for(current.sample.size in sample.size.coverage) {
            
            counter.alt = counter.alt + 1
            
            # a short message on screen
            message(".", appendLF = FALSE)
            if(counter.alt %% 64 == 0) {
                message("\n", appendLF = FALSE)
            }
            
#            # sampling N times from the original text
#            if(parallel_mode == TRUE) {
#                # a loop involving many cores, to extract text samples in N iterations 
#                test.table = foreach::foreach(i = 1:iterations, .combine = "rbind") %dopar% get.vector.of.freqs(get.test.text)
#            } else {
                # a loop using one CPU core: a classic solution
                test.table = c()
                for(i in 1:iterations) {
                    g = get.vector.of.freqs(get.test.text)
                    test.table = rbind(test.table, g)
                }
#            }
            
            rownames(test.table) = paste(test.text, 1:iterations, sep="_")    
            
            # another loop (the main one!), aka classification
            # which involves different vectors of features
 #           if(parallel_mode == TRUE) {
 #               # this version involves many CPU cores
 #               classify_results = foreach::foreach(f = mfw) %dopar% perform.classification(f)
 #           } else {
                # ...and this is a one-core equivalent of the above
                classify_results = list()
                no_of_f = 0
                for(f in mfw) {
                    no_of_f = no_of_f + 1
                    b = perform.classification(f)
                    classify_results[[no_of_f]] = b
                }
#            }
            
            # retrieving the names of the classes used in the prediction stage
            predicted_classes = colnames(classify_results[[1]])
            
            # retrieving accuracies from the results (stored as an attribute)
            accuracy = sapply(classify_results, function(x) attr(x, "accuracy"))
            # scaling the compact accuracy values as well (see above)
            accuracy = accuracy / iterations
            
            # computing the Simpson's index of diversity
            class_diversity = sapply(classify_results, function(x) get.dispersion(x))
            
            # reshaping the results
            classify_results = t(sapply(classify_results, rbind))
            rownames(classify_results) = mfw
            colnames(classify_results) = predicted_classes
            
            
            # collecting the results
            accuracy_all = cbind(accuracy_all, accuracy)
            diversity_all = cbind(diversity_all, class_diversity)
            confusion_matrices_all[[counter.alt]] = classify_results
        
        }
        
        
        
        # reshaping the confusion matrices, so that they are stored as tables
        counter_i = 0
        confusion_matrices_current_text = list()
        for(z in 1:length(confusion_matrices_all[[1]][,1]) ) {
            counter_i = counter_i +1
            r = sapply(confusion_matrices_all, function(x) x[z,])
            colnames(r) = sample.size.coverage
            confusion_matrices_current_text[[counter_i]] = r
        }
        names(confusion_matrices_current_text) = paste("mfw", mfw, sep = "_")
        
        
        
        # naming the accuracy results' rows and columns
        rownames(accuracy_all) = paste("mfw", mfw, sep = "_")
        colnames(accuracy_all) = sample.size.coverage
        
        # naming the class diversity results
        rownames(diversity_all) = paste("mfw", mfw, sep = "_")
        colnames(diversity_all) = sample.size.coverage
        
        # adding the current scores to the joint object
        joint.accuracy.scores[[iteration.counter]] = accuracy_all
        joint.diversity.scores[[iteration.counter]] = diversity_all
        joint.confusion.matrices[[iteration.counter]] = confusion_matrices_current_text
    
    }
    
    message("")
    
    # attaching some names to the variable containing the results
    names(joint.accuracy.scores) = test.texts
    names(joint.diversity.scores) = test.texts
    names(joint.confusion.matrices) = test.texts
    
    
    
    # simplifying the names of the output variables
    accuracy.scores = joint.accuracy.scores
    diversity.scores = joint.diversity.scores
    confusion.matrices = joint.confusion.matrices
    
    
    
    
    if(exists("accuracy.scores")) {
        attr(accuracy.scores, "description") = "accuracy scores for the tested texts"
    }
    if(exists("diversity.scores")) {
        attr(diversity.scores, "description") = "Simpson's index of diversity for the tested texts"
    }
    if(exists("confusion.matrices")) {
        attr(confusion.matrices, "description") = "all classification scores (raw tables)"
    }
    if(exists("test.texts")) {
        attr(test.texts, "description") = "names of the tested texts"
    }

    
    # creating an object (list) that will contain the final results,
    results = list()
    # elements that we want to add on this list
    variables.to.save = c("accuracy.scores", 
                          "diversity.scores", 
                          "confusion.matrices",
                          "test.texts")
    # checking if they really exist; getting rid of non-existing ones:
    filtered.variables = ls()[ls() %in% variables.to.save]
    # adding them to the list
    for(i in filtered.variables) {
        results[[i]] = get(i)
    }


    
    # adding some information about the current function call
    # to the final list of results
    results$call = match.call()
    results$name = call("size.penalize")
    class(results) = c("sample.size", "stylo.results")
    
    
    return(results)
    
}






