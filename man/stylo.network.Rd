\name{stylo.network}
\alias{stylo.network}
\title{Bootstrap consensus networks, with D3 visualization}
\description{
A function to perform Bootstrap Consensus Network analysys (Eder, 2017),
supplemented by interactive visualization (this involves javascript D3). 
This is a variant of the function \code{\link{stylo}}, except that it 
produces final networks without any external software (e.g. Gephi).
To use this function, one is required to install the package \code{networkD3}.
}
\usage{
stylo.network(mfw.min = 100, mfw.max = 1000, ...)
}
\arguments{
  \item{mfw.min}{the minimal MFW value (e.g. 100 most frequent words) to 
    start the bootstrap procedure with.}

  \item{mfw.max}{the maximum MFW value (e.g. 1000 most frequent words), 
    where procedure should stop. It is required that at least three
    iterations are completed.}

  \item{...}{any variable produced by \code{stylo.default.settings}
    can be set here, in order to overwrite the default values. An example
    of such a variable is \code{network = TRUE} (switched off as default)
    for producing stylometric bootstrap consensus networks (Eder, forthcoming); 
    the function saves a csv file, containing a list of nodes that can be 
    loaded into, say, Gephi.}
}
\details{
The Bootstrap Consensus Network method computes nearest neighborship relations 
between texts, and then tries to represent them in a form of a network 
(Eder, 2017).  Since multidimensional methods are sensitive to input features 
(e.g. most frequent words), the methdod produces a series of networks 
for different MFW settings, and then combines them into a consensus network. 
To do so, it assumes that both the mininum MFW value and the maximum value
is provided.
If no additional argument is passed, then the function tries to load
text files from the default subdirectory \code{corpus}. 
There are a lot of additional options that should be passed to this
function; they are all loaded when \code{stylo.default.settings} is 
executed (which is typically called automatically from inside the \code{stylo} 
function).
}
\value{
The function returns an object of the class \code{stylo.results}:
a list of variables, including a table of word frequencies, vector of features 
used, a distance table and some more stuff. Additionally, depending on which 
options have been chosen, the function produces a number of files containing 
results, plots, tables of distances, etc.
}
\references{
Eder, M. (2017). Visualization in stylometry: cluster analysis using networks. 
    "Digital Scholarship in the Humanities", 32(1): 50-64.
}

\author{Maciej Eder}

\seealso{
\code{\link{stylo}}
}
\examples{
\dontrun{
# standard usage (it builds a corpus from a set of text files):
stylo.networks()

# to take advantage of a dataset provided by the library 'stylo',
# in this case, a selection of Amarican literature from the South
data(lee)
help(lee) # to see what this dataset actually contains
# 
stylo.network(frequencies = lee)
}
}
%\keyword{principal components analysis}
%\keyword{multidimensional scaling}
%\keyword{cluster analysis}
%\keyword{bootstrap consensus trees}
%\keyword{dendrogram}
%\keyword{scatterplot}
