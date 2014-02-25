\name{stylo.default.settings}
\alias{stylo.default.settings}
\title{Setting variables}
\description{
Function that sets a series of global variables to be used by \code{stylo} 
and which can be modified by users via \code{gui.stylo}.
}
\usage{
stylo.default.settings(...)
}
\arguments{
  \item{...}{any variable as produced by this function
    can be set here to overwrite the default values.}
}
\details{
This function is typically called from inside \code{stylo} and \code{gui.gui}.
}
\value{
The function returns a list of ca. 50 variables. 
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
