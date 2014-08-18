# Delta in its various flavours

perform.delta = function(training.set, 
                         test.set, 
                         distance = "CD",
						 z.scores.both.sets = TRUE) {
#


no.of.variables = length(training.set[1,])


#if(length(training.set[1,]) != no.of.variables) {
#stop("!!!")
#}


# calculating z-scores either of training set, or of both sets
if(z.scores.both.sets == FALSE) {
  # mean and standard dev. for each word (in training set)
  training.set.mean = c(sapply(as.data.frame(training.set), mean))
  training.set.sd = c(sapply(as.data.frame(training.set), sd))
  # function for z-scores scaling executed for training.set
  zscores.training.set = scale(training.set)
  rownames(zscores.training.set) = rownames(training.set)
  # function for z-scores scaling executed for test.set
  zscores.test.set = 
            scale(test.set, center=training.set.mean, scale=training.set.sd)
  rownames(zscores.test.set) = rownames(test.set)
  # the two tables with calculated z-scores should be put together
  zscores.table.both.sets = rbind(zscores.training.set, zscores.test.set)
} else {
  # the z-scores can be calculated on both sets as alternatively  
  zscores.table.both.sets = scale(rbind(training.set, test.set))
  zscores.table.both.sets = zscores.table.both.sets[,]
}


# some distances require just freqs
freq.table.both.sets = rbind(training.set, test.set)



# calculating classic Delta distances
if(distance == "CD") {
  distance.table = 
            as.matrix(dist(zscores.table.both.sets,
            method="manhattan")) / no.of.variables
  }
# calculating Argamon's "Linear Delta"
if(distance == "AL") {
  distance.table = 
            as.matrix(dist(zscores.table.both.sets,
            method="euclidean")) / no.of.variables
  }
# calculating Delta distances with Eder's modifications
if(distance == "ED") {
  zscores.plus.e.value = t( t(zscores.table.both.sets)*((1+(no.of.variables:1))/no.of.variables) )
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
# replaces the names of the samples (the extension ".txt" is cut off)
#rownames(distance.table)=gsub("\\.txt$","",rownames(zscores.table.both.sets))
#colnames(distance.table)=gsub("\\.txt$","",rownames(zscores.table.both.sets))

# #################################################
# extracting candidates, drawing, printing, etc.

# a selected area of the distance.table is needed, with colnames()
no.of.possib = length(training.set[,1])
no.of.candid = length(test.set[,1])
selected.dist = 
          as.matrix(distance.table[no.of.possib+1:no.of.candid,1:no.of.possib])

return(selected.dist)
}

