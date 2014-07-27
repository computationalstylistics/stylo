\name{gui.oppose}
\alias{gui.oppose}
\title{GUI for the function oppose}
\description{
Graphical user interface for \code{oppose}. This function sets most of the variables needed for \code{oppose}.
}
\usage{
gui.oppose(...)
}
\arguments{
  \item{...}{any variable as produced by \code{stylo.default.settings}
    can be set here to overwrite the default values.}
}
\details{
The function calls \code{stylo.default.settings} to initialize a number
of default variables. Then it reads the file \code{oppose_config.txt} (if the file
exists and can be found in the current directory) to overwrite any default values.
Then a GUI box appears, allowing the variables' customization by the user. Refer to 
HOWTO available at \url{https://sites.google.com/site/computationalstylistics/} 
for a detailed explanation what the particular variables are for 
and how to use them.
}
\value{
The function returns a list containing ca. 50 variables. 
}
\author{
Jan Rybicki, Maciej Eder
}
\seealso{
\code{\link{oppose}}, \code{\link{stylo.default.settings}}
}
\examples{
\dontrun{
gui.oppose()

my.variables = gui.oppose()
summary(my.variables)
}
}
%\keyword{GUI}
