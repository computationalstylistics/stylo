
#rbind(primary.set[,1:mfw],secondary.set[,1:mfw])


crossv = function(training.set, 
                  test.set,
                  cv.folds = 10,
                  classes.training.set = NULL,
                  classes.test.set = NULL,
                  classification.method = "delta",
#                  distance.measure = "delta",
#                  show.features = FALSE,
#                  no.of.candidates = 3, 
                  ...) {
#

add.args = list(...)



primary.set = training.set
secondary.set = test.set
#how.many.correct.attributions = 3
#mfw=100
#current.culling = 0


#if(1 > 1) {

  message("\ncross-validation...\n")
 

  #bootstrap.output = "bootstrap_output.txt"
  # cleaning the bootstrapfile
  #cat("",file=bootstrap.output,append=F)

  # creating an empty matrix for the final success scores
  cross.validation.results = c()
  cross.validation.results.all = c()




  # beginning of k-fold cross-validation (k being the number of iterations)
  for(iterations in 1 : cv.folds) {

    # an additional table combined of frequencies of set I and II
    # just for feeding the bootstrap module 
    freq.table.both.sets.binded = rbind(primary.set,secondary.set)


    names.of.training.set.orig = rownames(primary.set)
    classes.training.set = gsub("_.*", "", rownames(primary.set))
    classes.test.set = gsub("_.*", "", rownames(secondary.set))
    names.both.sets = rownames(freq.table.both.sets.binded)
    classes.both.sets = c(classes.training.set, classes.test.set)
    
    training.samples = c()
    test.samples = c()

    
      # this looks for the classes that were not represented so far in I set
      for(i in names(table(classes.training.set)) ) {
        #
        # count the number of samples of class i included originally in I set
        no.of.training.samples = sum(as.numeric(classes.training.set == i))
        # determine the class' name, surround the name with word boundary char
        class.name = paste("\\b",i,"\\b",sep="")
        # in both sets, identify the positions of current class' samples 
        pinpoint.samples = grep(class.name, classes.both.sets)
        # sanity check, just in case
        if(length(pinpoint.samples) > no.of.training.samples) {
                # select randomly N items from the pinpoited positions
                training = sample(pinpoint.samples, no.of.training.samples)
                # identify the remaining ones: future test set samples
                test = setdiff(pinpoint.samples, training)
                # pick the names at the positions identified above
                training.samples = c(training.samples, names.both.sets[training])
                # the remaining ones go to the test set
                test.samples = c(test.samples, names.both.sets[test])
        } else {
                test = pinpoint.samples
                test.samples = c(test.samples, names.both.sets[test])
        }
      }


#### !!! Anon samples are excluded!!!


  # establishing the training set:
  training.set = freq.table.both.sets.binded[training.samples,]

  # establishing the test set
  test.set = freq.table.both.sets.binded[test.samples,]

  



  if(tolower(classification.method) == "delta") {
    classification.results = perform.delta(training.set, test.set, ...)
  }
  if(tolower(classification.method) == "knn") {
    classification.results = perform.knn(training.set, test.set, ...)
  }
  if(tolower(classification.method) == "svm") {
    classification.results = perform.svm(training.set, test.set, ...)
  }
  if(tolower(classification.method) == "nsc") {
    classification.results = perform.nsc(training.set, test.set, ...)
  }
  if(tolower(classification.method) == "naivebayes") {
    classification.results = perform.naivebayes(training.set, test.set, ...)
  }

  
  # retrieving classes of the new training set
  classes.training = gsub("_.*","",rownames(training.set))
  
  # retrieving classes of the new test set
  classes.test = gsub("_.*","",rownames(test.set))


  
#    # returns the number of correct attributions
#    if(how.many.correct.attributions == TRUE) {
          no.of.correct.attrib = sum(as.numeric(classes.test == 
                                     classification.results))
#      # getting the max. number of samples that couold be guessed
      perfect.guessing.cv = sum(as.numeric(classes.test %in% classes.training))
#      cat("\n",file=outputfile,append=T)
#      cat(mfw, " MFW , culled @ ",current.culling,"%,  ",
#               no.of.correct.attrib," of ", perfect.guessing.cv,"\t(",
#               round(no.of.correct.attrib / perfect.guessing.cv * 100, 1),"%)",
#               "\n",file=outputfile,append=T,sep="")
      # percentage of correct attributions
      success.rate.cv = no.of.correct.attrib / perfect.guessing.cv * 100
      # combining results for k folds
      cross.validation.results = c(cross.validation.results, success.rate.cv)
    }

#  }

  
  cross.validation.results.all = cbind(cross.validation.results.all, cross.validation.results)
#  colnames(cross.validation.results.all) = paste(mfw, "@", current.culling, sep="")
  
  
  
  return(cross.validation.results.all)
}   # <-- if(cv.folds > 0)


