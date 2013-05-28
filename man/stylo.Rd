\name{stylo}
\alias{stylo}
\alias{stylo.package}
\title{Stylometric multidimensional analyses}
\description{It is quite a long story what this function does. Basically,
    it is an all-in-one tool for a variety of experiments in computational
    stylistic. For a more detailed description, refer to miniHOWTO.}

\usage{
stylo()

stylo(gui = TRUE, path = "", corpus.dir = "corpus")
}

\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{corpus.dir}{a subdirectory (within a working directory) that
    contains corpus files; if not specified, the subdirectory \code{corpus} 
    will be used.}
}

\details{There is a lot of additional options that should be passed to this
    function; they are all loaded when \code{stylo.default.settings} is 
    executed (it will be invoked automatically from inside the 
    \code{stylo} function).}

\value{The function produces a number of files containing final results,
    plots, tables of distances, and so on (depending on which options 
    have been chosen).}

\references{Eder, M. and Rybicki, J. (2011). Stylometry with R. In: 
    \code{Digital Humanities 2011: Conference Abstracts}. Stanford University,
    Stanford, pp. 308-11.}

\author{Maciej Eder, Jan Rybicki, Mike Kestemont}

\seealso{
 \code{\link{classify}}, \code{\link{oppose}}, \code{\link{rolling.delta}}
}

\examples{
  \dontrun{
  #
  # standard usage:
  stylo()
  #
  # batch mode, custom name of corpus directory:
  stylo(gui = FALSE, corpus.dir = "ShakespeareCanon")
  #
  }
}

\keyword{principal components analysis}
\keyword{multidimensional scaling}
\keyword{cluster analysis}
\keyword{bootstrap consensus trees}
