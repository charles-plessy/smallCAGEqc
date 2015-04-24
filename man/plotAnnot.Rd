\name{plotAnnot}
\alias{plotAnnot}
\title{
  Plot annotation statistics
}
\description{
  Using a data frame containing mapping statistics in counts, plot the data as
  percentages in stacked barplots.
}
\usage{
  plotAnnot(LIBS, SCOPE, TITLE)
}
\arguments{
  \item{LIBS}{
    A data frame with columns named \code{promoter}, \code{exon}, \code{intron}
    \code{mapped}, \code{extracted}, \code{rdna}, and \code{tagdust}, that will be
    passed to the \code{mapStats} function.
}
  \item{SCOPE}{
    The value on which to normalise (see \code{mapStats}).
}
  \item{TITLE}{
    The title of the plot.
}
}
\details{
  Stacked barplots with error bars inspired from
  <http://stackoverflow.com/questions/10417003/stacked-barplot-with-errorbars-using-ggplot2>.
  See <http://www.biomedcentral.com/1471-2164/14/665/figure/F1> for example.
}

\author{
  Charles Plessy <plessy@riken.jp>
}

\seealso{
  \code{\link{hierarchAnnot}}, \code{\link{loadLogs}}, \code{\link{mapStats}}
}

\examples{
## plotAnnot(libs, 'annotation', 'Here is the title')
}

\keyword{ ~annotation }
\keyword{ ~plot }
\keyword{ ~CAGE }
