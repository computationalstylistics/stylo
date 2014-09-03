\name{gui.classify}
\alias{gui.classify}
\title{GUI for the function classify}
\description{
Graphical user interface for \code{classify}. Via the GUI, this function can set
most of the variables needed for \code{classify}.
}
\usage{
gui.classify(...)
}
\arguments{
  \item{...}{any variable as produced by \code{stylo.default.settings}
    can be set here to overwrite the default values.}
}
\details{
The function calls \code{stylo.default.settings} to initialize a number
of default variables. Then it reads the file \code{classify_config.txt} 
(if the file exists and can be found in the current directory) to 
overwrite any default values. Then a GUI box appears, allowing the variables' 
customization by the user. Refer to HOWTO available at 
\url{https://sites.google.com/site/computationalstylistics/} 
for a detailed explanation what the particular variables are for 
and how to use them.
}
\value{
The function returns a list containing ca. 100 variables. 
}
\author{
Jan Rybicki, Maciej Eder
}
\seealso{
\code{\link{classify}}, \code{\link{gui.stylo}}
}
\examples{
\dontrun{
gui.classify()

my.variables = gui.classify()
summary(my.variables)
}
}
%\keyword{GUI}
