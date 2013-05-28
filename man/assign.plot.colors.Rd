\name{assign.plot.colors}
\alias{assign.plot.colors}
\title{Assign colors to samples}
\description{
  Function that assigns unique colors to each class represented in a corpus: 
  it is used for graph auto-coloring.
}
\usage{
  assign.plot.colors(x, col = "colors")
}
\arguments{
  \item{x}{a vector containing names of samples represented in a corpus; it is 
    essential to use underscore as a class delimiter. Consided the following 
    examples: c("Sterne_Tristram", "Sterne_Sentimental", "Fielding_Tom", ...), 
    where the classes are the authors' names, and c("M_Joyce_Dubliners", 
    "F_Woolf_Night_and_day", "M_Conrad_Lord_Jim", ...), where the classes 
    are M and F coded according to authors' gender.}
  \item{col}{an optional argument indicating color palette: "colors" for 
    full-color output (default), "grayscale" for grayscale (might be usefull 
    for preparing publicable pictures), and "black", if one does not want 
    to use colors.}
}
\details{
  Function for graph auto-coloring; depending on the user's choice, 
  it assigns colors or grayscale tones to matching strings of characters 
  in texts' name (as a delimiter, the underscore character is used). 
  Alternatively, all the labels can be marked black.
}
\value{
  The function returns a vector of colors, using their conventional names 
  (e.g. \code{red}, \code{maroon4}, \code{mediumturquoise}, \code{gold4}, 
  \code{deepskyblue}, ...), or numeric values, when grayscale option was 
  chosen (e.g. \code{#000000}, \code{#000000}, \code{#595959}, 
  \code{#B2B2B2}, ...).
}
\author{
  Maciej Eder, Jan Rybicki, Mike Kestemont
}
\examples{
### in this example, three discrete classes will be established, 
# for Tacitus, Caesar, and Livius
#
sample.names = c("Tacitus_Annales","Tacitus_Germania","Tacitus_Histories",
                 "Ceasar_Civil_wars","Caesar_Gallic_wars",
                 "Livius_Ab_Urbe_Condita")
assign.plot.colors(sample.names)

# as above, but using grayscale:
#
assign.plot.colors(sample.names, col = "grayscale")
}
\keyword{scatterplot}
\keyword{dendrogram}
