\name{define.plot.area}
\alias{define.plot.area}
\title{Define area for scatterplots}
\description{
  Function that calculates the size of scatterplot, taking into consideration
  additional margin to fit longer labels of samples, optional margin defined
  by user, and some space to offset scatterplot labels from points.
}
\usage{
  define.plot.area(x.coord, y.coord, xymargins = 2, v.offset = 0)
}
\arguments{
  \item{x.coord}{a vector of x coordinates, optionally with names.}
  \item{y.coord}{a vector of y coordinates, optionally with names.}
  \item{xymargins}{additional margins (expressed in \% of actual plot area.}
  \item{v.offset}{label offset (in \%).}
}
\details{
  Function that finds out the coordinates of scatterplots. It computes 
  the extreme x and y values, adds some margins, and optionally extends
  the top margin if a plot uses sample labels. Automatic margin extention
  will take place only if the x and y coordinates were supplemented by
  their names (i.e. labels of points to be shown on scatterplot).
}
\author{
  Maciej Eder
}
\seealso{
  \code{\link{assign.plot.colors}}, \code{\link{stylo}} 
}
\examples{
\dontrun{ % this has to be done properly!!!!
define.plot.area(to.be.done)
}
}
\keyword{scatterplot}
