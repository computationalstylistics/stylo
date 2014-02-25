\name{define.plot.area}
\alias{define.plot.area}
\title{Define area for scatterplots}
\description{
Function that determines the size of a scatterplot, taking into consideration 
additional margin to fit longer labels appearing on a graph (if applicable), 
optional margin defined by user, and some space to offset scatterplot labels 
from points (if applicable).
}
\usage{
define.plot.area(x.coord, y.coord, xymargins = 2, v.offset = 0)
}
\arguments{
  \item{x.coord}{a vector of x coordinates, optionally with names.}
  \item{y.coord}{a vector of y coordinates.}
  \item{xymargins}{additional margins (expressed as a \% of the actual plot area).}
  \item{v.offset}{label offset (expressed as a \% of the actual plot area).}
}
\details{
  Function that finds out the coordinates of scatterplots: it computes 
  the extreme x and y values, adds margins, and optionally extends
  the top margin if a plot uses sample labels. Automatic margin extension
  will only take place if the x coordinates are supplemented by
  their names (i.e. labels of points to be shown on scatterplot).
}
\author{
Maciej Eder
}
\seealso{
\code{\link{assign.plot.colors}}, \code{\link{stylo}} 
}
\examples{
# to determine the plotting area for 4 points:
define.plot.area( c(1,2,3,4), c(-0.001,0.11,-0.023,0.09))

# to determine plot coordinates, taking into consideration 
# the objects' names
my.points = cbind(c(1,2,3,4),c(-0.001,0.11,-0.023,0.09))
rownames(my.points) = c("first","second","third","very_long_fourth")
define.plot.area(my.points[,1], my.points[,2])
}
%\keyword{scatterplot}
