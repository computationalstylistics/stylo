\name{gui.stylo}
\alias{gui.stylo}
\title{GUI for stylo}
\description{
Graphical user interface for the function \code{stylo}. This function sets most of the variables needed for \code{stylo}.
}
\usage{
gui.stylo(...)
}
\arguments{
  \item{...}{any variable as produced by \code{stylo.default.settings}
    can be set here to overwrite the default values.}
}
\details{
The function calls \code{stylo.default.settings} to initialize a number
of default variables. Then it reads the file \code{stylo_config.txt} (if the file
exists and can be found in the current directory) to overwrite any default values.
Then a GUI box appears, allowing the variables' customization by the user. Refer to 
HOWTO available at \url{https://sites.google.com/site/computationalstylistics/} 
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
\code{\link{stylo}}, \code{\link{stylo.default.settings}}
}
\examples{
\dontrun{
gui.stylo()

my.variables = gui.stylo()
summary(my.variables)
}
}
%\keyword{GUI}
