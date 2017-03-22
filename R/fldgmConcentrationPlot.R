#' fldgmConcentrationPlot
#' 
#' Plot histogram of cDNA concentrations after a C1 run
#' 
#' @param LIBS A metadata table, often called \sQuote{libs} table,
#'        that contains at least a \sQuote{Concentration} and a \sQuote{Run}
#'        column.  The table will be coerced as a data frame
#'        (without checking names), so that Bioconductor's DataFrame
#'        objects can also be used.#'        
#' @param scales The \sQuote{scales} argument for the plot.
#'        Default: \sQuote{fixed}.
#' @param group A formula to be passed to \sQuote{ggplot2}'s
#'        \sQuote{facet_wrap} function.
#'        
#' @examples 
#' # Example data
#' libs <- read.table(system.file("extdata/libs-with-all-metadata.tsv", package="smallCAGEqc"))
#' 
#' # Plot a single histogram
#' fldgmConcentrationPlot(libs)
#' 
#' # Faced by group
#' fldgmConcentrationPlot(libs, group = ~group)
#' 
#' # Test coercion to data frame.
#' if(require(S4Vectors))
#'   fldgmConcentrationPlot(DataFrame(libs))
#' 
#' @import ggplot2
#' @export fldgmConcentrationPlot

fldgmConcentrationPlot <- function ( LIBS
                                   , scales="fixed"
                                   , group=~Run) {
  LIBS <- data.frame(LIBS, check.names = FALSE)
  qplot( data   = LIBS
       , Concentration
       , geom   = "histogram"
       , xlab   = "Concentration (ng/μL)"
       , ylab   = "Count"
       , colour = Run
       # Suppress annoying message about defaults of binwidth
       , binwidth = diff(range(LIBS$Concentration))/30) +
    facet_wrap(group, scales=scales)
}
