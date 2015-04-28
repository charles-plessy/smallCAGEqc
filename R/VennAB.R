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
