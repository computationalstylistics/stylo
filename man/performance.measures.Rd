\name{performance.measures}
\alias{performance.measures}
\title{Accuracy, Precision, Recall, and the F Measure}
\description{
This function returns a few standard measurments used to test how efficient
a given classifier is, in a supervised machine-learnig classification setup.}
\usage{
performance.measures(expected_classes, predicted_classes, f_beta = 1)
}
\arguments{
  \item{expected_classes}{a vector of expected classes, or the classification
  results that we knew in advance.}

  \item{predicted_classes}{a vector of predictions outputted from a classifier.}
  
  \item{f_beta}{the F score is usually used in its F1 version, but one can
      use any other scaling factor, e.g. F(1/2) or F(2); the default value
      is 1.}
}
\value{
The function returns a vector of four values: accuracy, precision, recall
and the F measure.
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

performance.measures(what_we_expected, what_we_got)


# classifying the standard 'iris' dataset:
data(iris)
x = subset(iris, select = -Species)
train = rbind(x[1:25,], x[51:75,], x[101:125,])
test = rbind(x[26:50,], x[76:100,], x[126:150,])
train.classes = c(rep("s",25), rep("c",25), rep("v",25))
test.classes = c(rep("s",25), rep("c",25), rep("v",25))
results = perform.delta(train, test, train.classes, test.classes)

performance.measures(test.classes, results)


}
%\keyword{distance measure}
