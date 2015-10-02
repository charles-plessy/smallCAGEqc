#' fldgmConcentrationPlot
#' 
#' Plot histogram of cDNA concentrations after a C1 run
#' 
#' @param LIBS A \sQuote{libs} table, that contains at least a
#'        \sQuote{Concentration} and a \sQuote{Run} column
#' @param scales The \sQuote{scales} argument for the plot.
#'        Default: \sQuote{fixed}.

fldgmConcentrationPlot <- function ( LIBS
                                   , scales="fixed") {
  qplot( data   = LIBS
       , Concentration
       , geom   = "histogram"
       , xlab   = "Concentration (ng/μL)"
       , ylab   = "Count"
       , colour = Run
       # Suppress annoying message about defaults of binwidth
       , binwidth = diff(range(LIBS$Concentration))/30) +
    facet_wrap( ~Run, scales=scales) +
    theme_bw()
}
