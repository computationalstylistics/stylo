# impostors, cf. Kestemont


# change the name of train and test set



perform.impostors = function(candidate.set, 
                         impostors.set,
                         iterations = 100,
                         features = 50,  # proportion of features to be assessed
                         impostors = 30,
                         classes.candidate.set = NULL,
                         classes.impostors.set = NULL,
                         distance = "delta",
# to be excluded!
                         z.scores.both.sets = TRUE
                         ) {




# getting the number of features (e.g. MFWs)
no.of.cols = length(candidate.set[1,])

# checking if the two sets are of the same size
if(length(impostors.set[1,]) != no.of.cols) {
        stop("candidate set and impostors set must have the same number of variables!")
}


# assigning classes, if not specified
if(length(classes.candidate.set) != length(rownames(candidate.set))) {
        classes.candidate.set = c(gsub("_.*", "", rownames(candidate.set)))
}

if(length(classes.impostors.set) != length(rownames(impostors.set))) {
        classes.impostors.set = c(gsub("_.*", "", rownames(impostors.set)))
}

#




# what if the number of desired impostors is bigger than the coprus



score = 0

for(k in 1:iterations) {



# extracting the features: the percentage passed as an argument
# is first converted into an integer
no.of.features = round(length(candidate.set[1,]) * features /100)

# extracting the selection of features from the colnames of one of the sets
feature.subset = sample(colnames(candidate.set))[1:no.of.features]

# extracting a subset of column (= current features) from both sets 
training.set = candidate.set[,feature.subset]
test.set = impostors.set[,feature.subset]

# picking randomly a specified number of texts written by the impostors
current.impostors = sample(rownames(test.set), impostors)
test.set = test.set[current.impostors,]




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



# sanitazing the input dataset, in order to avoid all zeros in a column
input.freq.table = input.freq.table[,colSums(input.freq.table) != 0]




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






# if factor(classes.training.set) != 2, then complain

# an extremely complicated way of getting the class that has 1 sample in the I set
anon = names(table(classes.candidate.set))[table(classes.candidate.set) == 1]
# target author, or the class that is not the anonymous text
target.author = setdiff(classes.candidate.set, anon)
# getting only the distances between the anon text and the corpus
distances = distance.table[,gsub("_.*", "", colnames(distance.table)) == anon]
# getting rid of the distance between the anon text and itself
distances = distances[distances > 0]

# get the nearest neighbor
nearest = sort(distances)[1]

# testing if the nearest author is in the target gropup
if(gsub("_.*", "", names(nearest)) == target.author) {
    score = score + 1/iterations
}


}


return(1 - score)
}

