#' exportInEnv
#' 
#' Takes R variables and exprort them as environment variables
#' 
#' This function was developed for analysis pipelines running R and shell chunks
#' with knitR.  Changing the environment in one shell chunk has no effect on the
#' next ones, but changing it in a R chunk has a global effect.
#' 
#' This function uses \code{.Internal(Sys.setenv()} because its syntax was more
#' friendly for automation. 
#'
#' @param ... Names of variables.
#'
#' @return 
#' Returns shell commands that, would have the same effect.  This is useful when
#' cut-pasting from knitR workflows that use R to change the environment and then
#' run some shell chunks.
#'
#' @examples 
#' foo <- "bar"
#' toto <- "tata"
#' exportInEnv("foo", "toto")

exportInEnv <- function(...) {

  exportOneVarInEnv <- function(VAR) {
    Name  <- VAR
    Value <- get(VAR)
    Message <- paste0( "export "
                      , paste(Name, Value, sep='=')
                      , "\n")
    .Internal(Sys.setenv(Name, Value))
    cat(Message)
  }

  l_ply( c(...)
       , exportOneVarInEnv
       )
}
