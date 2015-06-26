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
