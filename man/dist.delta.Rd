\name{dist.delta}
\alias{dist.delta}
\alias{dist.argamon}
\alias{dist.eder}
\title{Delta Distance}
\description{
Function for computing Delta similarity measure of a matrix of values,
e.g. a table of word frequencies. Apart from the Classic Delta, two other 
flavors of the measure are supported: Argamon's Delta and Eder's Delta. 
There are also non-Delta distant measures available: see e.g. 
\code{\link{dist.cosine}} and \code{\link{dist.simple}}.}
\usage{
dist.delta(x, scale = TRUE)

dist.argamon(x, scale = TRUE)

dist.eder(x, scale = TRUE)
}
\arguments{
  \item{x}{a matrix or data table containing at least 2 rows and 2 cols,
  the samples (texts) to be compared in rows, the variables in columns.}
  
  \item{scale}{the Delta measure relies on scaled frequencies -- if you have 
  your matrix scaled already (i.e. converted to z-scores), switch this option 
  off. Default: TRUE.}
}
\value{
The function returns an object of the class \code{dist}, containing distances
between each pair of samples. To convert it to a square matrix instead,
use the generic function \code{as.dist}.
}
\references{
Argamon, S. (2008). Interpreting Burrows's Delta: geometric and 
    probabilistic foundations. "Literary and Linguistic Computing", 
    23(2): 131-47.

Burrows, J. F. (2002). "Delta": a measure of stylistic difference and 
    a guide to likely authorship. "Literary and Linguistic Computing", 
    17(3): 267-87.

Eder, M. (2015). Taking stylometry to the limits: benchmark study on 5,281 
    texts from Patrologia Latina. In: "Digital Humanities 2015: Conference 
    Abstracts" \url{http://dh2015.org/abstracts}.

Jannidis, F., Pielstrom, S., Schoch, Ch. and Vitt, Th. (2015). Improving 
    Burrows' Delta: An empirical evaluation of text distance measures.
    In: "Digital Humanities 2015: Conference Abstracts" 
    \url{http://dh2015.org/abstracts}.
}

\author{Maciej Eder}

\seealso{
\code{\link{stylo}}, \code{\link{classify}}, \code{\link{dist.cosine}}, 
\code{\link{as.dist}}
}
\examples{
# first, preparing a table of word frequencies
        Iuvenalis_1 = c(3.939, 0.635, 1.143, 0.762, 0.423)
        Iuvenalis_2 = c(3.733, 0.822, 1.066, 0.933, 0.511)
        Tibullus_1  = c(2.835, 1.302, 0.804, 0.862, 0.881)
        Tibullus_2  = c(2.911, 0.436, 0.400, 0.946, 0.618)
        Tibullus_3  = c(1.893, 1.082, 0.991, 0.879, 1.487)
        dataset = rbind(Iuvenalis_1, Iuvenalis_2, Tibullus_1, Tibullus_2, 
                        Tibullus_3)
        colnames(dataset) = c("et", "non", "in", "est", "nec")

# the table of frequencies looks as follows
        print(dataset)
        
# then, applying a distance
        dist.delta(dataset)
        dist.argamon(dataset)
        dist.eder(dataset)

# converting to a regular matrix
        as.matrix(dist.delta(dataset))

}
%\keyword{distance measure}
