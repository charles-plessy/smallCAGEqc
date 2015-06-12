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
