\name{stylo.default.settings}
\alias{stylo.default.settings}
\title{Setting variables}
\description{Funcion that sets a bunch of global variables to be used by
  \code{stylo} and modified using \code{gui.stylo}}

\usage{
  stylo.default.settings()
}
\details{
  This function is invoked from inside \code{stylo} and \code{gui.gui};
  it would be a rather weird idea to use it in other contexts.
}
\author{
  Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
  \code{\link{stylo}}, \code{\link{gui.stylo}}
}
\examples{
  stylo.default.settings()
  ls()
}
\keyword{variables}
