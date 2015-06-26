\name{setupForMoirai}
\alias{setupForMoirai}
\title{Setup environment for using Moirai results}
\description{
Takes informations about a library and a Moirai run, and export environment variables
pointing to Moirai results and describing the run.
}
\usage{
setupForMoirai(LIBRARY, MOIRAI_USER, MOIRAI_PROJECT)
}
\arguments{
  \item{LIBRARY}{Library name.}
  \item{MOIRAI_USER}{Moirai user name.}
  \item{MOIRAI_PROJECT}{Moirai project name.}
%%  \item{GROUP_SHARED}{(Optional.) Root directory for annontation and gene symbols file.}
%%  \item{GENE_SYMBOLS}{(Optional.) Path to gene symbols file.}
%%  \item{ANNOTATION}{(Optional.)Path to annotation file.}
}

\details{
Using LIBRARY, MOIRAI_USER and MOIRAI_PROJECT, \code{setupForMoirai} will find
the directory containing the last Moirai run for that library, and export its
path in the environment variable PROCESSED_DATA.

It will also set the environment variables GROUP_SHARED, GENE_SYMBOLS and
ANNOTATION, to default values.
}

\value{
Prints shell commands that, would have the same effect.  This is useful when
cut-pasting from knitR workflows that use R to change the environment and then
run some shell chunks.

Returns PROCESSED_DATA.
}

\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
\code{\link{exportInEnv}}, \code{\link{loadLogs}}
}

\examples{
## PROCESSED_DATA <- setupForMoirai(
##   LIBRARY        = '140609_M00528_0024_000000000-A8CBW',
##   MOIRAI_USER    = 'nano-fluidigm',
##   MOIRAI_PROJECT = 'C1_CAGE' )
}

\keyword{ ~CAGE }
\keyword{ ~Moirai }% __ONLY ONE__ keyword per line
