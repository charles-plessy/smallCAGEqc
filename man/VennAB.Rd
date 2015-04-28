\name{VennAB}
\alias{VennAB}
\title{
  Venn diagram beween two sets A and B}
\description{
  This is a simple function wrapping \code{venn.diagram()} function from
  the VennDiagram package, with a standard arbitrary look and feel.
}

\usage{
VennAB( A_NAME
      , A
      , B_NAME
      , B
      , MAIN
      , SUB )
}

\arguments{
  \item{A_NAME}{
    The name of the first set (character string).
}
  \item{A}{
    The first set (vector).
}
  \item{B_NAME}{
    The name of the second set (character string).
}
  \item{B}{
    The second set (vector).
}
  \item{MAIN}{
    The main title (character string).
}
  \item{SUB}{
    The subtitle under the main title (character string).
}
}

\details{
  The venn diagram is plotted in grid.draw()
}

\value{
  Plots the diagram without returning a value.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

\examples{
## VennAB( "Name of group A", groupA
##       , "Name of group B", groupB
##       , "Main title"
##       , "subtitle")
}

\keyword{ ~CAGE }
