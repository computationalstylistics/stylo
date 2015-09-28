\name{galbraith}
\alias{galbraith}
\docType{data}
\title{
Table of word frequencies for .............
}
\description{
This dataset contains a selection of 6 novels in English, written by Jane Austen ("Emma", "Pride and Prejudice", "Sense and Sensibility"), Anne Bronte ("Agnes Grey", "The Tenant of Wildfell Hall"), and Emily Bronte ("Wuthering Heights").
}
\usage{data("galbraith")}

\details{
The novels are represented as elements of a class \code{stylo.corpus}, i.e. a list containing particular texts as its elements. The texts are not tokenized.
}
\source{
The texts are harvested from open-access resources, e.g. the Gutenberg Project.
}
\examples{
data(galbraith)

print(galbraith)
summary(galbraith)

}
\keyword{datasets}
