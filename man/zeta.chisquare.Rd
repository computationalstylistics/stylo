\name{zeta.chisquare}
\alias{zeta.chisquare}
\title{Compare two subcorpora using a home-brew variant of Craig's Zeta}
\description{
This is a function for comparing two sets of texts; unlike keywords analysis,
it this method the goal is to split input texts into equal-sized slices, 
and to check the appearence of particular words over the slices. Number 
of slices in which a given word appeared in the subcorpus A and B is then
compared using Chisquare (if p value exceeds 0.05, a difference is considered
significant). This method is based on original Zeta as developed
by Burrows and extended by Craig (Burrows 2007, Craig and Kinney 2009).
}
\usage{
zeta.chisquare(input.data)
}
\arguments{
\item{input.data}{a matrix of two columns.}

}

\value{
The function returns a list of two elements: the first contains words (or 
other units, like n-grams) statistically preferred by the authors
of the primary subcorpus, while the second element contains words of avoided. 
Since this measure is symmetrical, the preferred words are ipso facto 
avoided by the secondary authors, and vice versa.
}
\author{
Maciej Eder
}
\seealso{
\code{\link{oppose}}, \code{\link{zeta.eder}},  
\code{\link{zeta.craig}}
}
\examples{
\dontrun{
zeta.chisquare(input.data, filter.threshold)
}
}
%\keyword{corpus comparison}
