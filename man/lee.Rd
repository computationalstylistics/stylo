\name{lee}
\alias{lee}
\docType{data}
\title{
Table of word frequencies (Lee, Capote, Faulkner, Styron, etc.)
}
\description{
This dataset contains a table (matrix) of relative frequencies of 3000 most 
frequent words retrieved from 28 books by 8 authors, including both novels 
by Harper Lee, namely "To Kill a Mockingbird" and "Go Set a Watchman". 
The remaining authors are as follows: Truman Capote ("In Cold Blood", 
"Breakfast at Tiffany's", "Summer Crossing", "The Grass Harp", 
"Other Voices, Other Rooms"), William Faulkner ("Absalom, Absalom!", 
"As I Lay Dying", "Light in August", "Go down, Moses", 
"The Sound and the Fury"), Ellen Glasgow ("Phases of an Inferior Planet", 
"Vein of Iron", "Virginia"), Carson McCullers ("The Heart is a Lonely Hunter", 
"The Member of the Wedding", "Reflections in a Golden Eye"), 
Flannery O'Connor ("Everything That Rises Must Converge", 
"The Compete Stories", "Wise Blood"), William Styron ("Sophie's Choice", 
"Set This House on Fire", "The Confessions of Nat Turner"), Eudora Welty 
("Delta Wedding", "Losing Battles", "The Optimist's Dauther").
}
\usage{data("lee")}

\details{
The word frequencies are represented as a two-dimensional table: variables 
(words) in columns, samples (novels) in rows. The frequencies are relative, 
i.e. the number of occurrences of particular word type was divided by 
the total number of tokens in a given text.
}
\source{
The novels represented by this dataset are protected by copyright. For that 
reason, it was not possible to provide the actual texts. Instead, 
the frequences of the most frequent words are obtained -- and those 
can be freely distributed.
}
\examples{
data(lee)
rownames(lee)

\dontrun{
stylo(frequencies = lee, gui = FALSE)
}
}
\keyword{datasets}
