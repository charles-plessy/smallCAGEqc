#` plotAnnot
#'
#' Plot annotation statistics
#' 
#' Using a data frame containing mapping statistics in counts, plot the data as
#' percentages in stacked barplots.
#' 
#' Stacked barplots with error bars inspired from
#' <http://stackoverflow.com/questions/10417003/stacked-barplot-with-errorbars-using-ggplot2>.
#' See <http://www.biomedcentral.com/1471-2164/14/665/figure/F1> for example.
#' 
#' @param LIBS A data frame with columns named \code{promoter}, \code{exon}, \code{intron}
#' \code{mapped}, \code{extracted}, \code{rdna}, and \code{tagdust}, that will be
#' passed to the \code{mapStats} function.
#' 
#' @param SCOPE The value on which to normalise (see \code{mapStats}).
#' 
#' @param TITLE The title of the plot.
#' 
#' @seealso \code{\link{hierarchAnnot}}, \code{\link{loadLogs}}, \code{\link{mapStats}}
#' 
#' @import ggplot2
#' 
#' @examples
#' libs <- read.csv( system.file("extdata", "libs.csv", package = "smallCAGEqc")
#'                 , row.names = 1)
#' p <- plotAnnot(libs, 'qc', 'Here is the title', libs$Error)
#' \dontrun{
#' print(p)
#' p + ggplot2::theme_bw()
#' ggplot2::theme_set(ggplot2::theme_bw()) ; p
#' }

plotAnnot <- function(LIBS, SCOPE, TITLE, GROUP="default") {
  ggplot( mapStats(LIBS, scope=SCOPE, group=GROUP)
        , aes( x    = group
             , y    = value
             , fill = variable)
        , main = all) +
    geom_bar(stat="identity") +
    geom_segment(aes( xend = group
                    , y    = ystart
                    , yend = yend)) +
    geom_point( aes( x = group
                   , y = yend)
              , shape = "|"
              , show_guide = FALSE) +
    coord_flip() +
    ggtitle(TITLE)
}
