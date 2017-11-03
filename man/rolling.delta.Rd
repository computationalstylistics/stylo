\name{rolling.delta}
\alias{rolling.delta}
\title{Sequential stylometric analysis}
\description{
Function that analyses collaborative works and tries to determine
the authorship of their fragments.
}
\usage{
rolling.delta(gui = TRUE, path = NULL, primary.corpus.dir = "primary_set",
              secondary.corpus.dir = "secondary_set")
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical user interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current working directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{primary.corpus.dir}{the subdirectory (within the current working 
    directory) that contains a collection of texts written by the authorial 
    candidates, likely to have been involved in the collaborative work 
    analyzed. If not specified, the default subdirectory  \code{primary_set} 
    will be used.}

  \item{secondary.corpus.dir}{the subdirectory (within the current working directory) that
    contains the collaborative work to be analyzed. If not specified, 
    the default subdirectory \code{secondary_set} will be used.}
}
\details{
The procedure provided by this function analyses collaborative works 
and tries to determine the authorship of their fragments. The first step 
involves a "windowing" procedure (Dalen-Oskam and Zundert, 2007) 
in which each reference text is segmented into consecutive, equal-sized 
samples or windows. After "rolling" through the test text, we can plot 
the resulting series of Delta scores for each reference text in a graph.
}
\value{
The function returns an object of the class \code{stylo.results}, and 
produces a final plot.
}
\references{
Eder, M., Rybicki, J. and Kestemont, M. (2016). Stylometry with R: a package 
    for computational text analysis. "R Journal", 8(1): 107-21.

van Dalen-Oskam, K. and van Zundert, J. (2007). Delta for Middle Dutch: 
    author and copyist distinction in Walewein. "Literary and Linguistic
    Computing", 22(3): 345-62.

Hoover, D. (2011). The Tutor's Story: a case study of mixed authorship.
    In: "Digital Humanities 2011: Conference Abstracts". Stanford University,
    Stanford, CA, pp. 149-51.

Rybicki, J., Kestemont, M. and Hoover D. (2014). Collaborative authorship: 
    Conrad, Ford and rolling delta. "Literary and Linguistic Computing",
    29(3): 422-31.
    
Eder, M. (2015). Rolling stylometry. "Digital Scholarship in the Humanities", 
    31(3): 457-69.

}

\author{Mike Kestemont, Maciej Eder, Jan Rybicki}

\seealso{
\code{\link{rolling.classify}}, \code{\link{stylo}}
}
\examples{
\dontrun{
# standard usage:
rolling.delta()

# batch mode, custom name of corpus directories:
rolling.delta(gui = FALSE, primary.corpus.dir = "MySamples",
       secondary.corpus.dir = "ReferenceCorpus")
}
}
%\keyword{corpus linguistics}
