\name{hannot}
\alias{hannot}
\title{Hierarchical annotation of the CAGE clusters}
\description{
  Parse an vector of annotations in character format an returns a vector of
  annotations as factors, where one single keyword has been selected from a
  provided list or the default list (see below).
}

\usage{hannot(annot, hierarchy="default")}

\arguments{
  \item{annot}{
A character vector of annotations.
}
  \item{hierarchy}{
Either the character dQuote{default}, or a data frame whith patterns in its
first column and keywords in its second column.  The default data frame is picked
when dQuote{default} is passed as argument.
}
}
\details{

The default data frame is as follows (as of version 0.2.1).

\preformatted{
             None      None
          snoscan     other
        antisense antisense
  intron,sense\\|    intron
    exon,sense\\|      exon
        lncRNA\\|      exon
         miRNA\\|     miRNA
         snRNA\\|     snRNA
        snoRNA\\|    snoRNA
          tRNA\\|      tRNA
}
}

\value{
Returns a vector of annotations as factors, where the levels are sorted in the
hierarchical order.  For instance, with the data frame above, if an annotation
matches for the keywords sQuote{miRNA} and sQuote{snoRNA}, then the reported
one will be sQuote{snoRNA}.
}

\author{Charles Plessy <plessy@riken.jp>}

\examples{
hannot(c('exon,sense|foo', 'foo', 'antisense of bar', 'intron,sense|bar', 'antisense of baz;snoRNA|SNORA70') )
}

\keyword{ ~CAGE }
\keyword{ ~annotation }% __ONLY ONE__ keyword per line
