% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fldgmPicoGreenStdCurve.R
\name{fldgmPicoGreenStdCurve}
\alias{fldgmPicoGreenStdCurve}
\title{fldgmPicoGreenStdCurve}
\usage{
fldgmPicoGreenStdCurve(FILE)
}
\arguments{
\item{FILE}{A standard Excel sheet containing fluorometric measurements of Fluidigm C1 cDNA yields.}
}
\value{
A data frame with standard DNA concentration in the first column, and fluorescence intensity in the second.
}
\description{
Reads standard curve from fluorometric measures entered in a template Excel sheet.
}
\examples{
## With a file called "1772-064-102.picogreen.xlsx":
##
## fldgmPicoGreenStdCurve(FILE="1772-064-102.picogreen.xlsx")

}
\seealso{
fldgmPicoGreen
}
