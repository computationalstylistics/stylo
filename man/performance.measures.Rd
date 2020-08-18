\name{performance.measures}
\alias{performance.measures}
\title{Accuracy, Precision, Recall, and the F Measure}
\description{
This function returns a few standard measurments used to test how efficient
a given classifier is, in a supervised machine-learnig classification setup.}
\usage{
performance.measures(predicted_classes, expected_classes = NULL, f_beta = 1)
}
\arguments{
  \item{predicted_classes}{a vector of predictions outputted from a classifier. 
  If an object containing results from \code{classify()}, \code{crossv},
  \code{perform.delta}, \code{perform.svm} etc. is provided, then no further
  input data is required (see below).}
  
  \item{expected_classes}{a vector of expected classes, or the classification
  results that we knew in advance. This argument is immaterial when an object
  of the class \code{"stylo.results"} is provided. In such a case, only the above
  parameter \code{predicted_classes} is obligatory.}
  
  \item{f_beta}{the F score is usually used in its F1 version, but one can
      use any other scaling factor, e.g. F(1/2) or F(2); the default value
      is 1.}
}
\value{
The function returns a list containing four performance indexes -- accuracy, precision, 
recall and the F measure -- for each class, as well as an average score for all classes.
}

\author{Maciej Eder}

\seealso{
\code{\link{classify}}, \code{\link{perform.delta}}, 
\code{\link{perform.svm}}, \code{\link{perform.nsc}}
}
\examples{

# classification results aka predictions (or, the classes "guessed" by a classifier)
what_we_got = c("prose", "prose", "prose", "poetry", "prose", "prose")
# expected classes (or, the ground truth)
what_we_expected = c("prose", "prose", "prose", "poetry", "poetry", "poetry")

performance.measures(what_we_got, what_we_expected)


# authorship attribution using the dataset 'lee'
#
data(lee)
results = crossv(training.set = lee, cv.mode = "leaveoneout", 
                 classification.method = "delta")
performance.measures(results)


# classifying the standard 'iris' dataset:
#
data(iris)
x = subset(iris, select = -Species)
train = rbind(x[1:25,], x[51:75,], x[101:125,])
test = rbind(x[26:50,], x[76:100,], x[126:150,])
train.classes = c(rep("s",25), rep("c",25), rep("v",25))
test.classes = c(rep("s",25), rep("c",25), rep("v",25))
results = perform.delta(train, test, train.classes, test.classes)

performance.measures(results)


}
%\keyword{distance measure}
