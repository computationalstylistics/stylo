# Delta in its various flavours

perform.delta = function(training.set, 
                         test.set,
                         classes.training.set = NULL,
                         classes.test.set = NULL,
                         distance = "CD",
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
freq.table.both.sets = rbind(training.set, test.set)





# calculating classic Delta distances
if(distance == "CD") {
  distance.table = 
            as.matrix(dist(zscores.table.both.sets,
            method="manhattan")) / no.of.cols
  }
# calculating Argamon's "Linear Delta"
if(distance == "AL") {
  distance.table = 
            as.matrix(dist(zscores.table.both.sets,
            method="euclidean")) / no.of.cols
  }
# calculating Delta distances with Eder's modifications
if(distance == "ED") {
  zscores.plus.e.value = t( t(zscores.table.both.sets) * ((1+(no.of.cols:1)) / no.of.cols) )
  distance.table = as.matrix(dist(zscores.plus.e.value,method="manhattan"))
  }
# calculating Eder's Simple distance to a matrix distance.table
if(distance == "ES") {
  distance.table = 
         as.matrix(dist(sqrt(freq.table.both.sets),method="manhattan"))
  }
# calculating Manhattan distance to a matrix distance.table
if(distance == "MH") {
  distance.table = 
           as.matrix(dist(freq.table.both.sets,method="manhattan"))
  }
# calculating Canberra distance to a matrix distance.table
if(distance == "CB") {
  distance.table = 
           as.matrix(dist(freq.table.both.sets,method="canberra"))
  }
# calculating Euclidean distance to a matrix distance.table
if(distance == "EU") {
  distance.table = 
           as.matrix(dist(freq.table.both.sets,method="euclid"))
  }
#




# selecting an area of the distance table containig test samples (rows),
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

