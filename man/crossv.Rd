\name{crossv}
\alias{crossv}
\title{Function to Perform Cross-Validation}
\description{
Function for performing a classification iteratively, while in each iteration
the composition of the train set and the test set is re-shuffled. There are
a few cross-validation flavors available; the current function supports (i)
stratified cross-validation, which means that in N iterations, the train/test
sets are assigned randomly, but the exact number of texts representing the 
original classes in the train set are keept unchanged; (ii) leave-one-out
cross-validation, which moves one sample from the train set to the test set, 
performs a classification, and then repeates the same procedure untill the
available samples are exhausted.
}
\usage{
crossv(training.set, test.set = NULL, 
       cv.mode = "leaveoneout", cv.folds = 10, 
       classes.training.set = NULL, classes.test.set = NULL, 
       classification.method = "delta", ...)
}
\arguments{
\item{training.set}{a table containing frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of text
          samples (for the training set). Make sure that
          the rows contain samples, and the columns -- variables
          (words, n-grams, or whatever needs to be analyzed).}

\item{test.set}{a table containing frequencies/counts for the training set. 
          The variables used (i.e. columns) must match the columns of the 
          training set. If the leave-one-out cross-validation flavor was 
          chosen, then the test set is not obligatory: it will be 
          created automatically. If the test set is present, however,
          it will be used as a "new" dataset for predicting its classes.
          It might seem a bit misleading -- new versions will distinguish
          more precisely the (i) train set, (ii) validation set and 
          (iii) test set in the strict sense.}

\item{cv.mode}{choose "leaveoneout" to perform leave-one-out 
          cross-validation; choose "stratified" to perform random selection
          of train samples in N iterations (see the \code{cv.folds} parameter 
          below) out of the all the available samples, provided that the very
          number of samples representing the classes in the original
          train set is keept in each iterations.}
          
\item{cv.folds}{the number of train/test set swaps, or cross-validation folds.
          A standard solution in the exact sciences seems to be a 10-fold
          cross-validation. It has been shown, however (Eder and Rybicki
          2013) that in text analysis setups, this might be not enough.
          This option is immaterial with leave-one-out cross-validation,
          since the number of folds is always as high as the number 
          of train samples.}

\item{classes.training.set}{a vector containing class identifiers for the
          training set. When missing, the row names of the training set
          table will be used; the assumed classes are the strings of 
          characters followed by the first underscore. Consider the following 
          examples: c("Sterne_Tristram",
          "Sterne_Sentimental", "Fielding_Tom", ...), where the classes
          are the authors' names, and c("M_Joyce_Dubliners",
          "F_Woolf_Night_and_day", "M_Conrad_Lord_Jim", ...), where the
          classes are M(ale) and F(emale) according to authors' gender.
          Note that only the part up to the first underscore in the
          sample's name will be included in the class label.}

\item{classes.test.set}{a vector containing class identifiers for the
          test set. When missing, the row names of the test set
          table will be used (see above).}

\item{classification.method}{the function invokes one of the classification
          methods provided by the package \code{stylo}. Choose one of the
          following: "delta", "svm", "knn", "nsc", "naivebayes".}

\item{...}{further parameters can be passed; they might be needed by 
          particular classification methods. See \code{perform.delta},
          \code{perform.svm}, \code{perform.nsc}, \code{perform.knn},
          \code{perform.naivebayes} for further results.}
}
\value{
The function returns a vector of accuracy scores across specified 
cross-validation folds. The attributes of the vector contain a list of 
misattributed samples (attr "misattributions") and a list of confusion
matrices for particular cv folds (attr "confusion_matrix").
}
\author{Maciej Eder}

\seealso{
\code{\link{perform.delta}}, \code{\link{perform.svm}}, 
\code{\link{perform.nsc}}, \code{\link{perform.knn}}, 
\code{\link{perform.naivebayes}}
}
\examples{
\dontrun{
crossv(training.set, test.set)
}

# classifying the standard 'iris' dataset:
data(iris)
x = subset(iris, select = -Species)
train = rbind(x[1:25,], x[51:75,], x[101:125,])
test = rbind(x[26:50,], x[76:100,], x[126:150,])
train.classes = c(rep("s",25), rep("c",25), rep("v",25))
test.classes = c(rep("s",25), rep("c",25), rep("v",25))

crossv(train, test, cv.mode = "stratified", cv.folds = 10, 
       train.classes, test.classes)


# text categorization

# specity a table with frequencies:
data(galbraith)
freqs = galbraith

# specify class labels:
training.texts = c("coben_breaker", "coben_dropshot", "lewis_battle", 
                   "lewis_caspian", "rowling_casual", "rowling_chamber", 
                   "tolkien_lord1", "tolkien_lord2")
train.classes = match(training.texts,rownames(freqs))

# select the training samples:
training.set = freqs[train.classes,]

# select remaining rows as test samples:
test.set = freqs[-train.classes,]

crossv(training.set, test.set)
}
%\keyword{distance measure}
