\name{exportInEnv}
\alias{exportInEnv}
\title{Export in environment}
\description{
Takes R variables and export them as environment variables.
}
\usage{
exportInEnv(...)
}
\arguments{
  \item{...}{Names of variables.}
}

\details{
This function was developed for analysis pipelines running R and shell chunks
with knitR.  Changing the environment in one shell chunk has no effect on the
next ones, but changing it in a R chunk has a global effect.

This function uses \code{.Internal(Sys.setenv()} because its syntax was more
friendly for automation. 
}

\value{
Returns shell commands that, would have the same effect.  This is useful when
cut-pasting from knitR workflows that use R to change the environment and then
run some shell chunks.
}

\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

foo <- "bar"
toto <- "tata"
exportInEnv("foo", "toto")

}

\keyword{ ~CAGE }
\keyword{ ~normalisation }% __ONLY ONE__ keyword per line
