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
#' @param LIBS A table with columns named \code{promoter}, \code{exon},
#'        \code{intron}, \code{mapped}, \code{extracted}, \code{rdna}, and
#'        \code{tagdust}, that will be passed to the \code{mapStats} function.
#'        The table will be coerced to a data frame, so that Bioconductor's
#'        DataFrame objects can be used.
#' @param SCOPE The value on which to normalise (see the plotAnnot vignette).
#' @param TITLE The title of the plot.
#' @param customScope A function passed to \code{\link{mapStats}} for the
#'        definition of custom scopes
#' @param normalise Whether to normalise or not. Default: TRUE.
#' 
#' @family smallCAGEqc annotation functions
#' @seealso \code{\link{loadLogs}}
#' 
#' @import ggplot2
#' @export plotAnnot
#' 
#' @examples
#' example(loadMoiraiStats)
#' p <- plotAnnot(libs, 'qc', 'Here is the title')
#' print(p)
#' p + ggplot2::theme_bw()
#' ggplot2::theme_set(ggplot2::theme_bw()) ; p
#' plotAnnot(libs, 'qc', 'Same, non-normalised', normalise = FALSE)
#' # Test coercion to data frame
#' if(require("S4Vectors"))
#'   plotAnnot(DataFrame(libs), 'qc', 'Here is the title')

plotAnnot <- function( LIBS
                     , SCOPE
                     , TITLE
                     , GROUP = "default"
                     , customScope = NULL
                     , normalise = TRUE) {
  
# Quick fix for backwards-incompatible change in ggplot 2
# The only difference is `position = position_stack(reverse = TRUE)`.
# See <https://github.com/tidyverse/ggplot2/issues/1837>
# No time for something beautiful.
  
LIBS <- data.frame(LIBS, check.names = FALSE)

if (packageVersion("ggplot2") >= "2.2.0") {
  ggplot( mapStats(LIBS, scope=SCOPE, group=GROUP, customScope = customScope, normalise = normalise)
        , aes( x    = group
             , y    = value
             , fill = variable)
        , main = all) +
    geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
    geom_segment(aes( xend = group
                    , y    = ystart
                    , yend = yend)) +
    geom_point( aes( x = group
                   , y = yend)
              , shape = "|") +
    coord_flip() +
    ggtitle(TITLE)
 } else {  
  ggplot( mapStats(LIBS, scope=SCOPE, group=GROUP, customScope = customScope, normalise = normalise)
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
              , shape = "|") +
    coord_flip() +
    ggtitle(TITLE)
}
}
