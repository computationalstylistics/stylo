\name{galbraith}
\alias{galbraith}
\docType{data}
\title{
Table of word frequencies (Galbraith, Rowling, Cobenm, Tolkien, Lewis)
}
\description{
This dataset contains a table (matrix) of relative frequencies of 3000 most 
frequent words retrieved from 26 books by 5 authors, including the novel 
"Cuckoo's Calling" by a mysterious Robert Galbraith that turned out 
to be J.K. Rowling. The remaining authors are as follows: 
Harlan Coben ("Deal Breaker", "Drop Shot", "Fade Away", "One False Move", 
"Gone for Good", "No Second Chance", "Tell No One"), C.S. Lewis 
("The Last Battle", "Prince Caspian: The Return to Narnia", 
"The Silver Chair", "The Horse and His Boy", "The Lion, the Witch and the Wardrobe", 
"The Magician's Nephew", "The Voyage of the Dawn Treader"), J.K. Rowling 
("The Casual Vacancy", "Harry Potter and the Chamber of Secrets", 
"Harry Potter and the Goblet of Fire", "Harry Potter and the Deathly Hallows", 
"Harry Potter and the Order of the Phoenix", "Harry Potter and the Half-Blood Prince", 
"Harry Potter and the Prisoner of Azkaban", "Harry Potter and the Philosopher's Stone"), 
and J.R.R. Tolkien ("The Fellowship of the Ring", "The Two Towers", 
"The Return of the King").
}
\usage{data("galbraith")}

\details{
The word frequencies are represented as a two-dimensional table: variables 
(words) in columns, samples (novels) in rows. The frequencies are relative, 
i.e. the number of occurrences of particular word type was divided by 
the total number of tokens in a given text.
}
\source{
The novels represented by this dataset are protected by copyright. 
For that reason, it was not possible to provide the actual texts. Instead, 
the frequences of the most frequent words are obtained -- and those 
can be freely distributed.
}
\examples{
data(galbraith)
rownames(galbraith)

\dontrun{
stylo(frequencies = galbraith, gui = FALSE)
}
}
\keyword{datasets}
