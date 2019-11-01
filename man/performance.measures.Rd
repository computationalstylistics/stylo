\name{performance.measures}
\alias{performance.measures}
\title{Accuracy, Precision, Recall, and the F Measure}
\description{
...................................   .}
\usage{
performance.measures(predicted_classes, actual_classes, 
                     beta = 1, positive = "1", 
                     drop_test_classes = TRUE )
}
\arguments{
  \item{predicted_classes}{.................................. .}
  
  \item{actual_classes}{.................................. .}
  
  \item{beta}{.................................. .}
  
  \item{positive}{no idea what it was supposed to mean.}
  
  \item{drop_test_classes}{............................ .}
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

# observed classes (or, the classes "guessed" by a classifier)
predicted = c("prose", "prose", "prose", "poetry", "prose", "prose")

# known classes (or, the ground truth)
actual = c("prose", "prose", "prose", "poetry", "poetry", "poetry")

performance.measures(predicted, actual)

}
%\keyword{distance measure}
