\name{make.table.of.frequencies}
\alias{make.table.of.frequencies}
\title{Prepare a table of relative word frequencies}
\description{Function that collects a bunch of frequency lists and puts 
them all in one table of frequencies; certainly, it means performing a number
of rearrangements inside particular lists in order to bind them in parallel.
The table is produced using a reference list of words (passed as an argument).
}
\usage{
make.table.of.frequencies(corpus, words, absent.sensitive = TRUE)
}
\arguments{
\item{corpus}{textual data: either a corpus (stored in a list), or 
a single text (vector); the data have to be split into words
(or other features, such as character n-grams or word pairs).}
\item{words}{a vector containig a reference word list that will be used
to build a table of frequencies (it is assumed that the reference list 
contains the same type of features as the corpus, e.g. words, character
n-grams, word pairs, etc.).}
\item{absent.sensitive}{this optional argument is used to prevent building 
tables of non-existing words. When switched on (default), the variables 
that contain but O values in all samples, will be excluded. Hovewer, in some 
cases this is important to keep all the variables regardless of their values. 
It is the case of comparing two corpora: even if a given word did not occur 
in the whole corpus A, it might have occured in the corpus B. In short: 
if you perform any analysis involving I and II set, switch this option to 
\code{FALSE}.}
}

\author{Maciej Eder, Jan Rybicki, Mike Kestemont}

\seealso{
\code{\link{load.corpus}}, \code{\link{load.corpus.and.parse}}
}

\examples{


# to get frequencies of (any) words from a single text:
sample.txt = txt.to.words("My father had a small estate 
    in Nottinghamshire: I was the third of five sons.")
make.table.of.frequencies(sample.txt, c("a", "the", "of"))
#
# to get table of frequencies across a number of texts:
txt.1 = "Gallia est omnis divisa in partes tres, quarum unam incolunt 
    Belgae, aliam Aquitani, tertiam qui ipsorum lingua Celtae, nostra 
    Galli appellantur."
txt.2 = "Si quis antea, iudices, mirabatur quid esset quod, pro tantis 
    opibus rei publicae tantaque dignitate imperi, nequaquam satis multi 
    cives forti et magno animo invenirentur qui auderent se et salutem 
    suam in discrimen offerre pro statu civitatis et pro communi 
    libertate, ex hoc tempore miretur potius si quem bonum et fortem 
    civem viderit, quam si quem aut timidum aut sibi potius quam rei 
    publicae consulentem."
txt.3 = "Nam mores et instituta vitae resque domesticas ac familiaris 
    nos profecto et melius tuemur et lautius, rem vero publicam nostri 
    maiores certe melioribus temperaverunt et institutis et legibus."
my.corpus.raw = list(txt.1, txt.2, txt.3)
my.corpus.clean = lapply(my.corpus.raw, txt.to.words)
my.favourite.words = c("et", "in", "se", "rara", "avis")
make.table.of.frequencies(my.corpus.clean, my.favourite.words)
#
# to include all the words from the reference list, no matter if they 
# occured in the corpus:
make.table.of.frequencies(my.corpus.clean, my.favourite.words,
    absent.sensitive=FALSE)
#
# to prepare a table of frequences of all the words represented in 
# a corpus, in descendent order of their occurrences:
complete.word.list = names(sort(table(unlist(my.corpus.clean)), 
    decreasing = TRUE))
make.table.of.frequencies(my.corpus.clean, complete.word.list)
#
# to create a table of frequencies of word pairs (word 2-grams):
my.word.pairs = lapply(my.corpus.clean, txt.to.features, ngram.size=2)
make.table.of.frequencies(my.word.pairs, c("et legibus", "hoc tempore"))
}


\keyword{text processing}
