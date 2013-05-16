\name{loadLogs}
\alias{loadLogs}
\title{Load mapping statistics from log files}
\description{
Loads mapping counts and other statistics produced during processing.
}
\usage{
loadLogs(source)
}
\arguments{
source: indicate to load the data from log files in the current directory, or from the summary
file of Moirai.
}
\details{

With \code{source='logs'}, \code{loadLogs} will load data from every file
ending in sQuote{\code{.log}} in the work directory.  Thes files are expected
contain tab-separated triples, with first the name of the mapping statistics,
like extracted, mapped, rdna, etc., then the sample identifier, and then the
number of reads.  \code{loadLogs} will crash or produce incorrect output if the
files do not contain triples, or if the sample identifiers are not matched
correctly in the files, or if the first word of the triples appears in multiple
files.

With \code{source='moirai'}, \code{loadLogs} will load data from a summary file
found in the \code{PROCESSED_DATA} directory.  It will also need the library
name to be in the \code{LIBRARY} variable.

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
\code{\link{hierarchAnnot}}, \code{\link{mapStats}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

\keyword{ ~CAGE }
\keyword{ ~annotation }% __ONLY ONE__ keyword per line
