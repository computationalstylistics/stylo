\name{assign.plot.colors}
\alias{assign.plot.colors}
\title{Assign colors to samples}
\description{
Function that assigns unique colors to each class represented in a corpus: used for graph auto-coloring.
}
\usage{
assign.plot.colors(labels, col = "colors")
}
\arguments{
\item{labels}{a vector containing the names of the samples in a corpus; 
it is obligatory to use an underscore as a class delimiter. Consider 
the following examples: c("Sterne_Tristram", "Sterne_Sentimental",
"Fielding_Tom", ...), where the classes are the authors' names, and 
c("M_Joyce_Dubliners", "F_Woolf_Night_and_day", "M_Conrad_Lord_Jim", ...), 
where the classes are M(ale) and F(emale) according to authors' gender. Note that only the part up to the first underscore in the sample's name will be included in the class label.}
\item{col}{an optional argument specifying the color palette to be used: "colors" for 
full-color output (default), "greyscale" for greyscale (useful 
for preparing publishable pictures), and "black", if no colors should be used.}
}
\details{
Function for graph auto-coloring; depending on the user's choice 
it assigns either colors or greyscale tones to matching strings of 
characters which stand for class identifiers. These
metadata will typically be encoded in the texts' filenames. (As class delimiter, the 
underscore character should be used). Alternatively, all labels can be plotted in black.
}
\value{
The function returns a vector of colors, using their conventional names 
(e.g. \code{red}, \code{maroon4}, \code{mediumturquoise}, \code{gold4}, 
\code{deepskyblue}, ...), or numeric values if the greyscale option was 
chosen (e.g. \code{#000000}, \code{#000000}, \code{#595959}, 
\code{#B2B2B2}, ...).
}
\author{
Maciej Eder
}
\examples{
# in this example, three discrete classes are specified, 
# for Tacitus, Caesar, and Livius
sample.names = c("Tacitus_Annales","Tacitus_Germania","Tacitus_Histories",
                 "Caesar_Civil_wars","Caesar_Gallic_wars",
                 "Livius_Ab_Urbe_Condita")
assign.plot.colors(sample.names)

# as above, but using greyscale:
assign.plot.colors(sample.names, col = "greyscale")
}
%\keyword{scatterplot}
%\keyword{dendrogram}
