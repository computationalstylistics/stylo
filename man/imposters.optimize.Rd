\name{imposters.optimize}
\alias{imposters.optimize}
\title{Tuning Parameters for the Imposters Method}
\description{
A function to optimize hyperparameters used in the General Imposters method
(see \code{link{imposters}} for further details). Using a grid search approach,
it tries to define a grey area where the attribution scores are not reliable.
}
\usage{
imposters.optimize(reference.set,
                   classes.reference.set = NULL,
                   parameter.incr = 0.01,
                   ...)
}
\arguments{
\item{reference.set}{a table containing frequencies/counts for several
          variables -- e.g. most frequent words -- across a number of texts
          written by different authors. Usually, it is a corpus of known authors
          (at least two tests per author) that is used to tune the optimal 
          hyperparameters for the imposters method. Such a tuning involves
          a leave-one-out procedure of identifying a gray area when the
          results returned by the classifier are not particularly reliable.
          E.g., if one gets 0.39 and 0.55 as the parameters, one would assume
          that any results of the \code{imposters()} function that lay within 
          this range should be claimed unreliable.
          Make sure that the rows contain samples, and the columns -- 
          variables (words, n-grams, or whatever needs to be analyzed).}

\item{classes.reference.set}{a vector containing class identifiers for the
          reference set. When missing, the row names of the set
          table will be used; the assumed classes are the strings of 
          characters followed by the first underscore. Consider the following 
          example: c("Sterne_Tristram",
          "Sterne_Sentimental", "Fielding_Tom", ...), where the classes
          are the authors' names.
          Note that only the part up to the first underscore in the
          sample's name will be included in the class label.}

\item{parameter.incr}{the procedure tries to optimize the hyperparameters
          via a grid search -- this means that it tests the range of values
          between 0 and 1 incremented by a certain fraction. If this is set 
          to 0.01 (default), it test 0, 0.01, 0.02, 0.03, ...}

\item{...}{any other argument that can be passed to the classifier; see 
          \code{perform.delta} for the parameters to be tweaked. In the current
          version of the function, only the distance measure used for computing 
          similarities between texts can be set. Available options so far: "delta" 
          (Burrows's Delta, default), "argamon" (Argamon's Linear Delta), 
          "eder" (Eder's Delta), "simple" (Eder's Simple Distance), 
          "canberra" (Canberra Distance), "manhattan" (Manhattan 
          Distance), "euclidean" (Euclidean Distance), "cosine"
          (Cosine Distance), "wurzburg" (Cosine Delta), "minmax" 
          (Minmax Distance, also known as the Ruzicka measure).
          }
}
\value{
The function returns two scores: the P1 and P2 values.
}
\references{
Koppel, M. , and Winter, Y. (2014). Determining if two documents are 
    written by the same author. "Journal of the Association for Information
    Science and Technology", 65(1): 178-187.

Kestemont, M., Stover, J., Koppel, M., Karsdorp, F. and Daelemans, W. (2016).
    Authenticating the writings of Julius Caesar. "Expert Systems With
    Applications", 63: 86-96.
}
\author{
Maciej Eder
}
\seealso{
\code{\link{imposters}}
}
\examples{
\dontrun{
# activating a dummy dataset, in our case: Harper Lee and her Southern colleagues
data(lee)

# running the imposters method against all the remaining authorial classes
imposters.optimize(lee)
}
}
%\keyword{classification}
