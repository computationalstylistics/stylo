

# dataset
# centroids
# dist. measure 


# !!!so far, the dist. measure is not flexible!!!


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

predicted.scores = c()
expected.scores = c()

#for(i in 1:length(reference.set[,1])) {
for(i in c(1:3,10:11)) {
    current.test.text = reference.set[i,]
    current.reference.set = reference.set[-c(i),]

    message(paste(rownames(reference.set)[i], " ..."))
    
    current.predicted = suppressMessages(imposters(reference.set = current.reference.set, 
                                    test = current.test.text, distance = "delta"))
    current.expected = as.numeric(classes.reference.set[i] == unique(classes.reference.set))
    
    predicted.scores = c(predicted.scores, current.predicted)
    expected.scores = c(expected.scores, current.expected)
    
}

#rownames(scores) = rownames(reference.set)




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




predicted = round(sapply(predicted.scores, rescale, p1 = 0.2, p2 = 0.9))
confusion.matrix = table(expected.scores, predicted)

# true negatives
TN = confusion.matrix[1,1]
# false negatives
FN = confusion.matrix[2,1]
# false positives
FP = confusion.matrix[1,2]
# true positives
TP = confusion.matrix[2,2]

# True positive rate (sensivity, recall)
TPR = TP / (TP + FN)
# False positive rate (fall-out)
FPR = FP / (FP + TN)




auc = c()

parameter.incr = 0.1

for(p1 in seq(0, 1, parameter.incr) ) {
    for(p2 in seq(p1, 1, parameter.incr) ) {
        predicted = round(sapply(predicted.scores, rescale, p1, p2))
        
        confusion.matrix = table(expected.scores, predicted)

# true negatives
TN = confusion.matrix[1,1]
# false negatives
FN = confusion.matrix[2,1]
# false positives
FP = tryCatch(confusion.matrix[1,2], error = function(e) FP = 0)
#FP = confusion.matrix[1,2]
# true positives
TP = tryCatch(confusion.matrix[2,2], error = function(e) TP = 0)
#TP = confusion.matrix[2,2]

# True positive rate (sensivity, recall)
TPR = TP / (TP + FN)
# False positive rate (fall-out)
FPR = FP / (FP + TN)

auc = rbind(auc, c(TPR, FPR))
        
        
    }
}



}
