\name{plotSlimSmear}
\alias{plotSlimSmear}
\title{
  Slimmer plots with a bit of data loss
}
\description{
  To reduce the size of SVG or PDF plots, this function rounds the data and
  remove the duplicate points at the same coordinates.
}
\usage{
  plotSlimSmear(COMP, MAIN, ROUND=2, PCH=19, CEX=1)
}
\arguments{
  \item{COMP}{A DGELRT object from edgeR.}
  \item{MAIN}{The title of the plot.}
  \item{ROUND}{The decimal for the rounding.}
  \item{PCH, CEX}{The \code{pch} and \code{cex} arguments passed to
the \code{points} function}
}
\details{
  Significantly over- and under-represented items are plotted in red and blue
  respectively.

  Using this function creates data loss.  Try rounding at different decimals and
  pick one that do not change the visual appearance of the plot significantly.
}

\author{
  Charles Plessy <plessy@riken.jp>
}

\seealso{
  \code{\link[edgeR]{plotSmear}}
}

\examples{
## plotSlimSmear(obj.comp, 'Here is the title')
}

\keyword{ ~edgeR }
\keyword{ ~plot }
