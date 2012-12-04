\name{loadLogs}
\alias{loadLogs}
\title{Load mapping statistics from log files}
\description{
Loads mapping statistics from a hard-coded list of files.
}
\usage{
loadLogs()
}
\arguments{
No arguments for the moment.
}
\details{
The files contain tab-separated triples, with first the name of the mapping
statistics, like extracted, mapped, rdna, etc., then the sample identifier,
and then the number of reads.

This function uses the \code{cast} function from the \code{reshape} package,
and for this reason this library depends on \code{reshape}.
}

\value{
Returns a data frame with one row per sample, and the following columns (if the
corresponding data is available).
  \item{sample}{Sample identifier (factor)}
  \item{extracted}{Number of extracted reads}
  \item{tagdust}{Number of reads containing oligonucleotide artefacts}
  \item{spikes}{Number of reads overlaping with the reference spike sequences}
  \item{rdna}{Number of reads overlaping with the reference ribosomal DNA sequences}
  \item{mapped}{Number of reads aligned to the reference genome}
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
\keyword{ ~annotation }% __ONLY ONE__ keyword per line
