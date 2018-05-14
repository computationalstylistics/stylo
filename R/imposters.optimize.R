

# dataset
# centroids
# dist. measure



imposters.optimize = function(reference.set = NULL,
                     classes.reference.set = NULL,
                     ...) {


setwd(tempdir())
library(stylo)
data(galbraith)
data(lee)
reference.set = lee
classes.reference.set = gsub("_.*", "", rownames(reference.set))

# more than one text per class
more.than.one = names(table(classes.reference.set)[table(classes.reference.set) > 1])

reference.set = reference.set[classes.reference.set %in% more.than.one , ]
classes.reference.set = classes.reference.set[classes.reference.set %in% more.than.one]

scores = c()

for(i in 1:length(reference.set[,1])) {
    current.test.text = reference.set[i,]
    current.reference.set = reference.set[-c(i),]
    
    xxxx = suppressMessages(imposters(reference.set = current.reference.set, test = current.test.text, distance = "manhattan"))
    
    cat(xxxx, "\n")
    scores = rbind(scores, xxxx)

}

rownames(scores) = rownames(reference.set)




rescale = function(score, p1, p2) {
  if(score < p1) {
      new_score = p1 * score
  } else if(score >= p1 & score <= p2) {
      new_score = 0.5
  } else {
      new_score = p2 + ((1 - p2) * score)
  }
  return(new_score)
}





parameter.incr = 0.1

for(p1 in seq(0, 1, parameter.incr) ) {
    for(p2 in seq(p1, 1, parameter.incr) ) {
        sapply(scores, rescale, p1 = 0.5, p2 = 0.4)
    }
}



}
