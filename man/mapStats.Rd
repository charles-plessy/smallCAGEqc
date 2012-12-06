\name{mapStats}
\alias{mapStats}
\title{Process mapping statistics}
\description{
Using a data frame containing mapping statistics in counts, transform the data in
percentages that can be used for stacked barplots.
}
\usage{
mapStats(libs, scope='all')
}
\arguments{
  \item{libs}{
A data frame with columns named \code{promoter}, \code{exon}, \code{intron}
\code{mapped}, \code{extracted}, \code{rdna}, and \code{tagdust}.
}
  \item{scope}{
The value on which to normalise. \dQuote{all} normalises on the number of extracted tags
and \dQuote{annotation} normalises on the number of aligned tags.
}
}
\details{
See http://stackoverflow.com/questions/10417003/stacked-barplot-with-errorbars-using-ggplot2 about stacked barplot.
}

\value{
Returns mean and standard deviation of normalised mapping statistics, plus absolute
positions for the error bars.
}

\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
\code{\link{hierarchAnnot}}, \code{\link{loadLogs}}
}

\examples{
## To be plotted with:
##
## \code{ggplot(mapstats, aes(x=group, y=value, fill=variable), main=LIBRARY) + geom_bar(stat="identity") + geom_segment(aes(xend = group,y = ystart, yend = yend)) +  geom_point(aes(x = group, y = yend),shape = "|",show_guide = FALSE) + coord_flip()  + opts(title=LIBRARY)}
}

\keyword{ ~CAGE }
\keyword{ ~annotation }
