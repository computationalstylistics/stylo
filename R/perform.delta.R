# Delta in its various flavours

perform.delta = function(training.set, 
                         test.set,
                         classes.training.set = NULL,
                         classes.test.set = NULL,
                         distance = "delta",
                         no.of.candidates = 3,
                         z.scores.both.sets = TRUE) {
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




# calculating z-scores either of training set, or of both sets
if(z.scores.both.sets == FALSE) {
  # mean and standard dev. for each word (in the training set)
  training.set.mean = c(sapply(as.data.frame(training.set), mean))
  training.set.sd = c(sapply(as.data.frame(training.set), sd))
  # z-scores for training.set
  zscores.training.set = scale(training.set)
  rownames(zscores.training.set) = rownames(training.set)
  # z-scores for test.set, using means and st.devs of the training set
  zscores.test.set = 
            scale(test.set, center=training.set.mean, scale=training.set.sd)
  rownames(zscores.test.set) = rownames(test.set)
  # the two tables with calculated z-scores should be put together
  zscores.table.both.sets = rbind(zscores.training.set, zscores.test.set)
} else {
  # the z-scores can be calculated on both sets as alternatively  
  zscores.table.both.sets = scale(rbind(training.set, test.set))
  # a dirty trick to get the values and nothing else
  zscores.table.both.sets = zscores.table.both.sets[,]
}


# some distances require just freqs
input.freq.table = rbind(training.set, test.set)





supported.measures = c("dist.euclidean", "dist.manhattan", "dist.canberra",
                       "dist.delta", "dist.eder", "dist.argamon",
                       "dist.simple", "dist.cosine")



# if the requested distance name is confusing, stop
if(length(grep(distance, supported.measures)) > 1 ) {
    stop("Ambiguous distance method: which one did you want to use, really?")

# if the requested distance name was not found invoke a custom plugin
} else if(length(grep(distance, supported.measures)) == 0 ){

    # first, check if a requested custom function exists 
    if(is.function(get(distance)) == TRUE) {
        # if OK, then use the value of the variable 'distance.measure' to invoke 
        # the function of the same name, with x as its argument
        distance.table = do.call(distance, list(x = input.freq.table))
        # check if the invoked function did produce a distance
        if(class(distance.table) != "dist") {
            # say something nasty here, if it didn't:
            stop("it wasn't a real distance measure function applied, was it?")
        }
    }

# when the chosen distance measure is among the supported ones, use it
} else {

    # extract the long name of the distance (the "official" name) 
    distance = supported.measures[grep(distance, supported.measures)]
    # then check if this is one of standard methods supported by dist()
    if(distance %in% c("dist.manhattan", "dist.euclidean", "dist.canberra")) {
         # get rid of the "dist." in the distance name
         distance = gsub("dist.", "", distance)
         # apply a standard distance, using the generic dist() function
         distance.table = as.matrix(dist(input.freq.table, method = distance))
    # then, check for the non-standard methods but still supported by Stylo
    } else if(distance %in% c("dist.simple", "dist.cosine")) {

         # invoke one of the distance measures functions from Stylo    
         distance.table = do.call(distance, list(x = input.freq.table))
    
    } else {
         # invoke one of the distances supported by 'stylo'; this is slightly
         # different from the custom functions invoked above, since it uses
         # another argument: z-scores can be calculated outside of the function
         distance.table = do.call(distance, list(x = zscores.table.both.sets, scale = FALSE))
    }
    
} 

# convert the table to the format of matrix
distance.table = as.matrix(distance.table)




# selecting an area of the distance table containing test samples (rows),
# contrasted against training samples (columns)
no.of.candid = length(training.set[,1])
no.of.possib = length(test.set[,1])
selected.dist = 
          as.matrix(distance.table[no.of.candid+1:no.of.possib,1:no.of.candid])
# assigning class ID to train samples
colnames(selected.dist) = classes.training.set
#



  if(no.of.candidates > length(classes.training.set)) {
          no.of.candidates = length(classes.training.set)
  }


# starting final variables
classification.results = c()
classification.scores = c()
classification.rankings = c()

for(h in 1:length(selected.dist[,1])) {
        ranked.c = order(selected.dist[h,])[1:no.of.candidates]
        current.sample = classes.training.set[ranked.c[1]]
        classification.results = c(classification.results, current.sample)
        #
        current.ranking = classes.training.set[ranked.c]
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

