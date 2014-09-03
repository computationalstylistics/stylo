\name{zeta.chisquare}
\alias{zeta.chisquare}
\title{Compare two subcorpora using a home-brew variant of Craig's Zeta}
\description{
This is a function for comparing two sets of texts; unlike keywords analysis,
it this method the goal is to split input texts into equal-sized slices, 
and to check the appearance of particular words over the slices. Number 
of slices in which a given word appeared in the subcorpus A and B is then
compared using standard chisquare test (if p value exceeds 0.05, a difference 
is considered significant). This method is based on original Zeta as developed
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
of the primary subcorpus, while the second element contains avoided words. 
Since the applied measure is symmetrical, the preferred words are ipso facto 
avoided by the secondary authors, and vice versa.
}
\references{
Burrows, J. F. (2007). All the way through: testing for authorship 
    in different frequency strata. "Literary and Linguistic Computing", 
    22(1): 27-48.

Craig, H. and Kinney, A. F., eds. (2009). Shakespeare, Computers, and the 
    Mystery of Authorship. Cambridge: Cambridge University Press.

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
