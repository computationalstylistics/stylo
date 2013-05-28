\name{stylo.default.settings}
\alias{stylo.default.settings}
\title{Setting variables}
\description{Funcion that sets a bunch of global variables to be used by
  \code{stylo} and modified using \code{stylo.gui}}

\usage{
  stylo.default.settings()
}
\details{
  This function is invoked from inside \code{stylo} and \code{stylo.gui};
  it would be a rather weird idea to use it in other contexts.
}
\author{
  Maciej Eder, Jan Rybicki, Mike Kestemont
}
\seealso{
  \code{\link{stylo}}, \code{\link{stylo.gui}}
}
\examples{
  stylo.default.settings()
  ls()
}
\keyword{variables}
