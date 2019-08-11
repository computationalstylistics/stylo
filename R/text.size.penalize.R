
text.size.penalize = function(training.frequencies = NULL, 
                              test.frequencies = NULL,
                              training.corpus = NULL, 
                              test.corpus = NULL,
                              mfw = c(100,200,300),
                              features = NULL, 
                              path = NULL, 
                              corpus.dir = "corpus",
                              sample.size.coverage = seq(100,10000,100),
                              sample.with.replacement = FALSE,
                              iterations = 100,
                              classification.method = "delta",
                              ...) {
    #
    
    add.args = list(...)
    
    
    
##### temporary!! ######    
corpus.language = "English.all"
list.cutoff = 1000
training.corpus = c("ABronte_Agnes", "ABronte_Tenant")
library(doMC)
library(foreach)
registerDoMC(cores = cpu.cores)
##### temporary!! ######    

# if(training.frequencies == NULL)

    input.texts = load.corpus.and.parse(files = "all", corpus.dir = corpus.dir, language = corpus.language)
    wordlist = make.frequency.list(input.texts, head = list.cutoff)
    doc.term.matrix = make.table.of.frequencies(corpus = input.texts, features = wordlist)
    
# else:     doc.term.matrix = training.frequencies

# if(training.corpus == NULL)
    test.texts = rownames(doc.term.matrix)
    
# else:     
    test.texts = training.corpus ## or names(training.corpus) !!!!!
    #
    
    # starting empty variables, to collect final results
    joint.attribution.scores = list()
    joint.confusion.matrices = list()
    iteration.counter = 0
    
    
    # an outer loop, to iterate over text samples
    for(test.text in test.texts) {
        
        # showing the currently processed sample on screen
        message(test.text)
        # setting a counter of iterations (= processed samples)
        iteration.counter = iteration.counter +1

    train.table = doc.term.matrix[rownames(doc.term.matrix) != test.text,]   

    
##### MAKE SURE THE TEXT EXISTS in the input.texts object!
##### if loaded from an external object, it has to be taken into account!
    get.test.text = input.texts[[grep(test.text, names(input.texts))]]
    
    # function (iterator) to get random samples from a given input text 
    get.vector.of.freqs = function(tokenized.text) {
        current.sample = sample(tokenized.text, size = current.sample.size, replace = sample.with.replacement)
        #current.sample = make.samples(tokenized.text, sampling = "random.sampling", sample.size = current.sample.size)[[1]]
        relative.frequencies = table(current.sample) / length(current.sample) * 100
        vector.of.freqs = relative.frequencies[wordlist]
        names(vector.of.freqs) = wordlist
        vector.of.freqs[which(is.na(vector.of.freqs))] = 0
        return(vector.of.freqs)
    }
    
    # function (iterator) to perform the classification stage
    perform.classification = function(mfw) {
        if(classification.method == "delta") {
            bbb = perform.delta(train.table[,1:mfw], test.table[,1:mfw], z.scores.both.sets = FALSE, ...)
        }
        if(classification.method == "svm") {
            bbb = perform.svm(train.table[,1:mfw], test.table[,1:mfw], ...)
        }
        if(classification.method == "nsc") {
            bbb = perform.nsc(train.table[,1:mfw], test.table[,1:mfw], ...)
        }
#### this class matching seems a bit weird!
        results = sum(as.numeric(bbb == gsub("_.*" ,"", test.text)))
        attr(results, "confusion_matrix") = attr(bbb, "confusion_matrix")
        return(results)
    }
    
    
    # starting a new (empty) variable
    compact.results = c()
    full.results = list()
    counter.alt = 0
    
    
    # now, iterating over specified range of sample sizes to asses
    for(current.sample.size in sample.size.coverage) {
        
        message(".", appendLF = FALSE)
        
        counter.alt = counter.alt + 1

##### ADJUST FOR #iterations (default = 100)
        
        # a loop involving many cores, to extract text samples in 100 iterations 
        test.table = foreach(i = 1:100, .combine = "rbind") %dopar% get.vector.of.freqs(get.test.text)
        rownames(test.table) = paste(test.text, 1:100, sep="_")    

        # another loop (the main one!), aka classification
        # which involves different vectors of features
        classify = foreach(f = mfw) %dopar% perform.classification(f)
        # collecting attr(ddd, "confusion_matrix") in the form of list
        confusion.matrix = list()
        for(g in 1:length(classify)) {
            confusion.matrix[[g]] = attr(classify[[g]], "confusion_matrix")
        }
        #
        names(confusion.matrix) = paste("mfw_", mfw, sep = "")
        # collecting the results
        compact.results = rbind(compact.results, unlist(classify) )
        full.results[[counter.alt]] = confusion.matrix
    
    }
    
    
    colnames(compact.results) = mfw
    rownames(compact.results) = sample.size.coverage
    joint.attribution.scores[[iteration.counter]] = compact.results
    
    names(full.results) = paste("sample_", sample.size.coverage, sep = "")
    joint.confusion.matrices[[iteration.counter]] = full.results
    
    }
    
    
    # attaching some names to the variable containing the results
    names(joint.attribution.scores) = test.texts
    names(joint.confusion.matrices) = test.texts

    
    
    
    return(compact.results)
    

}
