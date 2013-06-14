\name{oppose}
\alias{oppose}
\title{Contrastive analysis of texts}
\description{
Function that performs a contrastive analysis between two given sets 
of texts. It generates a list of words significantly preferred by 
a tested author (or, a bunch of authors), and another list containing 
the words significantly avoided. Some visualizations are available.
}
\usage{
oppose(gui = TRUE, path = "", primary.corpus.dir = "primary_set",
secondary.corpus.dir = "secondary_set", test.corpus.dir = "test_set")
}
\arguments{
  \item{gui}{an optional argument; if switched on, a simple yet effective 
    graphical interface (GUI) will appear. Default value is \code{TRUE}.}

  \item{path}{if not specified, the current directory will be used 
    for input/output procedures (reading files, outputting the results).}

  \item{primary.corpus.dir}{a subdirectory (within a working directory) that
    contains one or more texts by to be examined against a comparison corpus;
    it can be texts by a given author to be contrasted with other authors, 
    it can be texts by females to be contrasted by males, etc. If not 
    specified, the subdirectory \code{primary_set} will be used.}

  \item{secondary.corpus.dir}{a subdirectory (within a working directory) that
    contains a comparison corpus, or a pool of texts to be contrasted
    with texts from the primary corpus. If not specified, the subdirectory 
    \code{secondary_set} will be used.}

  \item{test.corpus.dir}{a subdirectory (within a working directory) that
    contains texts used to verify the discriminatory strength of the features
    extracted from the primary and the secondary sets; ideally, they should 
    be a few known texts belonging to both classes (e.g. written by female 
    and male authors). If not specified, the subdirectory \code{test_set} 
    will be used; it this subdirectory does not exist or does not contain
    any texts, the validation test will not be applied.}
}
\details{
It performs a contrastive analysis between two given sets of texts, 
using Burrows's Zeta (2007) in its different flavors, including Craig's 
extensions (Craig and Kinney, 2009). Also, the Whitney-Wilcoxon procedure
as introduced by Kilgariff (2001) is available. The function generates 
a vector of words significantly preferred by a tested author, and another 
vector containing the words significantly avoided.
}
\references{
Eder, M. Ketemont, M. and Rybicki, J. (2013). Stylometry with R:
    a suite of tools. In: "Digital Humanities 2013: Conference Abstracts". 
    University of Nebraska-Lincoln, Lincoln, NE (in press).

Burrows, J. F. (2007). All the way through: testing for authorship 
    in different frequency strata. "Literary and Linguistic Computing", 
    22(1): 27-48.

Craig, H. and Kinney, A. F., eds. (2009). Shakespeare, Computers, and the 
    Mystery of Authorship. Cambridge: Cambridge University Press.

Hoover, D. (2010). Teasing out authorship and style with t-tests and
    Zeta. In: "Digital Humanities 2010: Conference Abstracts". 
    King's College London, pp. 168-170.

Kilgariff A. (2001). Comparing Corpora. "International Journal of Corpus
    Linguistics" 6(1): 1-37.
}

\author{Maciej Eder, Jan Rybicki, Mike Kestemont}

\seealso{
\code{\link{stylo}}, \code{\link{classify}}, \code{\link{rolling.delta}}
}
\examples{
\dontrun{
# standard usage:
oppose()

# batch mode, custom name of corpus directories:
oppose(gui = FALSE, primary.corpus.dir = "ShakespeareCanon",
       secondary.corpus.dir = "MarloweSamples")
}
}
\keyword{zeta}
\keyword{corpus linguistics}
