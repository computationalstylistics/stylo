\name{define.plot.area}
\alias{define.plot.area}
\title{Define area for scatterplots}
\description{
Function that calculates the size of scatterplot, taking into consideration
additional margin to fit longer labels of samples, optional margin defined
by user, and some space to offset scatterplot labels from points (if applicable).
}
\usage{
define.plot.area(x.coord, y.coord, xymargins = 2, v.offset = 0)
}
\arguments{
  \item{x.coord}{a vector of x coordinates, optionally with names.}
  \item{y.coord}{a vector of y coordinates.}
  \item{xymargins}{additional margins (expressed in \% of actual plot area).}
  \item{v.offset}{label offset (in \% of actual plot area).}
}
\details{
  Function that finds out the coordinates of scatterplots. It computes 
  the extreme x and y values, adds some margins, and optionally extends
  the top margin if a plot uses sample labels. Automatic margin extension
  will take place only if the x coordinates were supplemented by
  their names (i.e. labels of points to be shown on scatterplot).
}
\author{
Maciej Eder
}
\seealso{
\code{\link{assign.plot.colors}}, \code{\link{stylo}} 
}
\examples{
# to establish plotting area for 4 given points:
define.plot.area( c(1,2,3,4), c(-0.001,0.11,-0.023,0.09))

# to find plot coordinates, taking into consideration objects' names
my.points = cbind(c(1,2,3,4),c(-0.001,0.11,-0.023,0.09))
rownames(my.points) = c("first","second","third","very_long_fourth")
define.plot.area(my.points[,1], my.points[,2])
}
\keyword{scatterplot}
