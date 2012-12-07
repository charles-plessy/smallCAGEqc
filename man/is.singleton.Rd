\name{is.singleton}
\alias{is.singleton}
\title{is.singleton}
\description{
Tests if each row of an expression table is a singleton.
}
\usage{
is.singleton(expression_table)
}
\arguments{
  \item{expression_table}{
An expression table (data frame or matrix), where all expression values are tag counts.
}
}
\details{
Returns TRUE if the cluster has only one tag in one library, or if it has no tags at all.  Otherwise,
returns FALSE.
}

\value{
A logical vector
}

\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rbind(c(1,1,1), c(0,0,1))
is.singleton(x)
}

\keyword{ ~CAGE }
