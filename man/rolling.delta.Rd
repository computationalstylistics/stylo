\name{rolling.delta}
\alias{rolling.delta}
\title{Sequential stylometric analysis}
\description{
Function for .... 
.............. ............... ............... ............
See: details.
}
\usage{
rolling.delta(gui = TRUE, path = "", primary.corpus.dir = "primary_set",
secondary.corpus.dir = "secondary_set")
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{primary.corpus.dir}{a subdirectory (within a working directory) that
    contains .....................; if not specified, the subdirectory 
    \code{primary_set} will be used.}

  \item{secondary.corpus.dir}{a subdirectory (within a working directory) that
    contains .....................; if not specified, the subdirectory 
    \code{secondary_set} will be used.}
}
\details{
The procedure provided by this function analyses collaborative works 
and tries to identify the authorship of their fragments. The first step 
involves a "windowing" procedure (Dalen-Oskam and Zundert, 2007) 
in which each reference text is segmented into consecutive, equal-sized 
samples or windows. After "rolling" through the test text we can plot 
the resulting series of Deltas for each reference text in a graph
}
\value{
The function produces ..............................
}
\references{
Eder, M. Ketemont, M. and Rybicki, J. (2013). Stylometry with R:
    a suite of tools. In: "Digital Humanities 2013: Conference Abstracts". 
    University of Nebraska-Lincoln, Lincoln, NE, pp. ???-???.

van Dalen-Oskam and van Zundert (2007).

Rybicki, Kestemont and Hoover (2013).
}

\author{Mike Kestemont, Jan Rybicki}

\seealso{
\code{\link{stylo}}, \code{\link{classify}}, \code{\link{oppose}}
}
\examples{
\dontrun{
#
# standard usage:
rolling.delta()
#
# batch mode, custom name of corpus directories:
rolling.delta(gui = FALSE, primary.corpus.dir = "MySamples",
       secondary.corpus.dir = "ReferenceCorpus")
#
}
}
\keyword{corpus linguistics}
