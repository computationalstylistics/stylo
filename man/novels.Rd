\name{novels}
\alias{novels}
\docType{data}
\title{
A selection of 19th-century English novels
}
\description{
This dataset contains a selection of 9 novels in English, written by Jane Austen 
("Emma", "Pride and Prejudice", "Sense and Sensibility"), Anne Bronte 
("Agnes Grey", "The Tenant of Wildfell Hall"), Charlotte Bronte ("Jane Eyre", 
"The Professor", "Villette"), and Emily Bronte ("Wuthering Heights").
}
\usage{data("novels")}

\details{
The novels are represented as elements of a class \code{stylo.corpus}, i.e. 
a list containing particular texts as its elements. The texts are not tokenized.
}
\source{
The texts are harvested from open-access resources, e.g. the Gutenberg Project.
}
\examples{
data(novels)

print(novels)
summary(novels)

}
\keyword{datasets}
