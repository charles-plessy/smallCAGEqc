countSymbols <- function(g2) {
  countSymbolsVector <- function(x) {
    if (all (x == 0))
      return (0)
    rownames(g2)[x >0] %>%
    listSymbols %>%
    length
  }

  lapply(g2, countSymbolsVector) %>%
  unlist
}
