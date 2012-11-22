\name{mapStats}
\alias{mapStats}
\title{Process mapping statistics}
\description{
Using a data frame containing mapping statistics in counts, transform the data in
percentages that can be used for stacked barplots.
}
\usage{
mapStats(libs)
}
\arguments{
  \item{libs}{
A data frame with columns named \code{promoter}, \code{exon}, \code{intron}
\code{Mapped}, \code{Extracted}, \code{rRNA}, and \code{TagDust}.
}
}
\details{
See http://stackoverflow.com/questions/10417003/stacked-barplot-with-errorbars-using-ggplot2 about stacked barplot.

To be plotted with:

\code{ggplot(mapstats, aes(x=Type, y=value, fill=variable), main=LIBRARY) + geom_bar(stat="identity") + geom_segment(aes(xend = Type,y = ystart, yend = yend)) +  geom_point(aes(x = Type, y = yend),shape = "|",show_guide = FALSE) + coord_flip()  + opts(title=LIBRARY)}
}

\value{
Returns mean and standard deviation of normalised mapping statistics, plus absolute
positions for the error bars.
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
