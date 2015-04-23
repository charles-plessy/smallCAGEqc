\name{fldgmPicoGreen}
\alias{fldgmPicoGreen}
\title{fldgmPicoGreen}
\description{
Reads Fluidigm C1 cDNA yields from fluorometric measures entered in a template Excel sheet.
}
\usage{
fldgmPicoGreen(FILE, TEMPLATE="PN 100-6260", FORMAT="long", RUN)
}
\arguments{
  \item{FILE}{Path to a file in Excel XML format.}
  \item{TEMPLATE}{A template identification number.}
  \item{FORMAT}{Long or wide.}
  \item{RUN}{
    A string representing the run name (for instance, the serial ID of the
    capture array).}
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
Returns a data frame indicating containing cDNA concentrations in nanograms per
microliter for each well.

In the "long" format, the data frame has 4 columns ("well", "row", "column" and
"concentration") and 96 rows, sorted by well from A01, A02, ... to H11, H12.
Row names are either well names, or run and well names, separated by an
underscore (\sQuote{_}).

With a "wide" format, returns a data frame with 8 rows (A to H) and 12 columns
(01 to 12).
}

\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## With a file called "1772-064-102.picogreen.xlsx":
##
## fldgmPicoGreen(FILE="1772-064-102.picogreen.xlsx"
##
## In command line:
##
## Rscript -e 'smallCAGEqc::fldgmPicoGreen(FILE="1772-064-102.picogreen.xlsx", TEMPLATE="PN 100-6260", RUN="1772-064-102")
}

\keyword{ ~CAGE }
\keyword{ ~C1 }% __ONLY ONE__ keyword per line
