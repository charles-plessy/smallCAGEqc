#' VennAB
#' 
#' Venn diagram beween two sets A and B
#' 
#' This is a simple function wrapping \code{venn.diagram()} function from
#' the VennDiagram package, with a standard arbitrary look and feel.
#' 
#' @param A_NAME The name of the first set (character string).
#' @param A The first set (vector).
#' @param B_NAME The name of the second set (character string).
#' @param B The second set (vector).
#' @param MAIN The main title (character string).
#' @param SUB The subtitle under the main title (character string).
#' 
#' The venn diagram is plotted in grid.draw().
#' 
#' @return Plots the diagram without returning a value.
#' 
#' @examples 
#' \dontrun{
#' VennAB( "Name of group A", groupA
#'       , "Name of group B", groupB
#'       , "Main title"
#'       , "subtitle")
#' }
#' 
#' @importFrom grid grid.draw
#' @importFrom VennDiagram venn.diagram
#' @export VennAB

VennAB <- function( A_NAME
                  , A
                  , B_NAME
                  , B
                  , MAIN
                  , SUB
                  )
{
  list_A_B        <- list(A,      B)
  names(list_A_B) <-    c(A_NAME, B_NAME)
  grid.draw(
     venn.diagram( list_A_B
                 , fill = c("aquamarine2", "darkblue") 
                 , cex = 1.5, cat.dist = 0.05, cat.cex = 1.4, lty =3
                 , fontfamily = "serif", fontface = "bold"
                 , main = MAIN, main.cex = 1.4, main.fontface = "bold"
                 , sub = SUB,   sub.cex  = 1.4, sub.fontface  = "bold"
                 , filename = NULL
                 )
  )
}
