\name{TPM}
\alias{TPM}
\title{Tags Per Million}
\description{
Takes a data frame of tag counts and return a data frame of tags per million.
}
\usage{
TPM(clusters)
}
\arguments{
  \item{clusters}{
A data frame of tag counts, where each row is a cluster and each column is a library.
}
}
\details{
Ensures that the column names of the returned data frame are identical to the input.
}

\value{
Returns a data frame expression values normalised in tags per million, where each row is a cluster and each column is a library.
}

\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

## The function is currently defined as
\keyword{ ~CAGE }
\keyword{ ~normalisation }% __ONLY ONE__ keyword per line
