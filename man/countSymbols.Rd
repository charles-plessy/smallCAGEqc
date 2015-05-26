\name{countSymbols}
\alias{countSymbols}
\title{Count unique gene symbols from a \dQuote{g2} table.}

\description{
Takes a \dQuote{g2} table and returns the number of detected genes for each
library.
}

\usage{
countSymbols(g2)
}

\arguments{
\item{g2}{A \dQuote{g2} table}
}

\details{
See \code{listSymbols} for details.
}

\value{
Returns the number of different symbols for each column of the \dQuote{g2} table.
}

\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
\code{\link{listSymbols}}
}

\examples{
g2 <- data.frame(sample1=c(1,2), sample2=c(1,0), row.names=c("A", "A,B"))
countSymbols(g2)
}

\keyword{ ~CAGE }
