\name{combnparallel}
\alias{combnomp}
\alias{combnsnow}
\alias{combnthrust}
\title{ Generate all combinations of the elements of x taken m at a time.}
\description{
             Generate all combinations of the elements of x taken m at a time.
             If x is a positive integer,  returns all combinations
             of the elements of seq(x) taken m at a time.
             If argument "fun" is not null,  applies a function given
             by the argument to each point.  If simplify is FALSE,  returns
             a list; else returns a vector or an array.  "..." are passed
             unchanged to function given by argument fun,  if any.

             combnomp: Allows user to choose between static, dynamic, and guided
             scheduling policies and any integer value for chunksize.
             The default is 'static' and a chunk size of 1.

             combnsnow: User must provide the number of clusters as the first
             argument.

         Implementations in OpenMP, R Snow, and Thrust.

}
\usage{
combnomp(x, m, fun=NULL, simplify=TRUE, sched=NULL, chunksize=NULL, ...)
}
combnsnow(cls, x, m, fun=NULL, simplify=TRUE, ...)
}
combnthrust(x, m, fun=NULL, simplify=TRUE, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{cls} { number of clusters for combnsnow }
  \item{x}{ vector source for combinations }
  \item{m}{ number of elements }
  \item{fun}{ function to be applied to each combination (may be null) }
  \item{simplify}{ logical, if FALSE, returns a list,
otherwise returns vector or array }
  \item { scheduling policy for combnomp }
  \item { chunk size value for combnomp }
  \item{\dots}{ args to fun }
}

\references{http://cran.r-project.org/web/packages/combinat/combinat.pdf}
\examples{
 combnomp(letters[1:4], 2)
 combnsnow(cls, 10, 5, min) # where cls is the number of clusters
 combnthrust(c(1:10), 3)
%\keyword{ combinatorics }
\keyword{ models }


