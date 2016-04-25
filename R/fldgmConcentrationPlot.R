#' fldgmConcentrationPlot
#' 
#' Plot histogram of cDNA concentrations after a C1 run
#' 
#' @param LIBS A \sQuote{libs} table, that contains at least a
#'        \sQuote{Concentration} and a \sQuote{Run} column
#' @param scales The \sQuote{scales} argument for the plot.
#'        Default: \sQuote{fixed}.
#' @param group A formula to be passed to \sQuote{ggplot2}'s
#'        \sQuote{facet_wrap} function.
#' 
#' @importFrom ggplot2 qplot facet_wrap
#' @export fldgmConcentrationPlot

fldgmConcentrationPlot <- function ( LIBS
                                   , scales="fixed"
                                   , group=~Run) {
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
