\name{perform.delta}
\alias{perform.delta}
\title{Distance-based classifier}
\description{
Delta: a simple yet effective machine-learning method of supervised classification,
introduced by Burrows (2002). It computes a table of distances between samples, 
and compares each sample from the test set against training samples, in order 
to find its nearest neighbor. Apart from classic Delta, a number of alternative 
distance measures are supported by this function.
}
\usage{
perform.delta(training.set, test.set, 
              classes.training.set = NULL, 
              classes.test.set = NULL, 
              distance = "delta", no.of.candidates = 3, 
              z.scores.both.sets = TRUE) }
\arguments{
\item{training.set}{a table containing frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of text
          samples (for the training set). Make sure that
          the rows contain samples, and the columns -- variables
          (words, n-grams, or whatever needs to be analyzed).}

\item{test.set}{a table containing frequencies/counts for the training set. 
          The variables used (i.e. columns) must match the columns of the 
          training set.}

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

\item{distance}{a kernel (i.e. a distance measure) used for computing 
          similarities between texts. Available options so far: "delta" 
          (Burrow's Delta, default), "argamon" (Argamon's Linear Delta), 
          "eder" (Eder's Delta), "simple" (Eder's Simple Distance), 
          "canberra" (Canberra Distance), "manhattan" (Manhattan 
          Distance), "euclidean" (Euclidean Distance), "cosine"
          (Cosine Distance).}

\item{no.of.candidates}{how many nearest neighbors will be computed for
          each test sample (default = 3).}

\item{z.scores.both.sets}{many distance measures convert input variables into 
          z-scores before computing any distances. Such a variable
          weighting is highly dependent on the number of input texts. One might
          choose either training set only to scale the variables, or the entire
          corpus (both sets). The latter is default.}
}
\value{
The function returns a vector of "guessed" classes: each test sample is
linked with one of the classes represented in the training set. Additionally,
final scores and final rankings of candidates are returned as attributes.
}
\references{
Argamon, S. (2008). Interpreting Burrows's Delta: geometric and 
  probabilistic foundations. "Literary and Linguistic Computing", 
  23(2): 131-47. 

Burrows, J. F. (2002). "Delta": a measure of stylistic difference and 
    a guide to likely authorship. "Literary and Linguistic Computing", 
    17(3): 267-87.

Jockers, M. L. and Witten, D. M. (2010). A comparative study of machine 
    learning methods for authorship attribution. "Literary and Linguistic
    Computing", 25(2): 215-23.


}
\author{
Maciej Eder
}
\seealso{
\code{\link{perform.svm}}, \code{\link{perform.nsc}},  
\code{\link{perform.knn}}, \code{\link{perform.naivebayes}}, 
\code{\link{dist.delta}}
}
\examples{
\dontrun{
perform.delta(training.set, test.set)
}

# classifying the standard 'iris' dataset:
data(iris)
x = subset(iris, select = -Species)
train = rbind(x[1:25,], x[51:75,], x[101:125,])
test = rbind(x[26:50,], x[76:100,], x[126:150,])
train.classes = c(rep("s",25), rep("c",25), rep("v",25))
test.classes = c(rep("s",25), rep("c",25), rep("v",25))

perform.delta(train, test, train.classes, test.classes)
}
%\keyword{classification}
