\name{stylo.default.settings}
\alias{stylo.default.settings}
\title{Setting variables}
\description{
Function that sets a bunch of global variables to be used by \code{stylo} 
and modified using \code{gui.stylo}.
}
\usage{
stylo.default.settings()
}
\details{
This function is invoked from inside \code{stylo} and \code{gui.gui};
it would be a rather weird idea to use it in other contexts.
}
\value{
The function returns a list containing some 50 elements, or particular
variables. 
}
\author{
Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
\code{\link{stylo}}, \code{\link{gui.stylo}}
}
\examples{
stylo.default.settings()

# to see which variables have been set:
names(stylo.default.settings())

# to use the elements of the list as if they were independent variables:
my.variables = stylo.default.settings()
attach(my.variables)
}
\keyword{variables}
