\name{fldgmPicoGreen}
\alias{fldgmPicoGreen}
\title{fldgmPicoGreen}
\description{
Reads Fluidigm C1 cDNA yields from fluorometric measures entered in a template Excel sheet.
}
\usage{
fldgmPicoGreen(FILE, TEMPLATE="PN 100-6260")
}
\arguments{
  \item{FILE}{Path to a file in Excel XML format.}
  \item{TEMPLATE}{A template identification number.}
}
\details{

After a C1 run, the PCR products are transferred to a 96-well plate and their
concentration can be quantified by a fluorometric (PicoGreen) assay in a
384-well plate.  Fluidigm provides template Excel sheets in which to paste the
raw results, that calculates concentrations using the standard curve,
replicates and blank wells of the 384-well-plate.

This function locates the results in the Excel sheet and returns them as a data
frame.

Only the templates PN 100-6260 and PN 100-6160 are supported, other templates (if any)
were not tested.
}

\value{
Returns a data frame representing a 96-well plate with 8 rows (A to H) and 12 columns
(01 to 12), containing cDNA concentrations in nanograms per microliter.
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

\keyword{ ~CAGE }
\keyword{ ~C1 }% __ONLY ONE__ keyword per line
