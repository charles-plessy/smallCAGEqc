countSymbols <- function(g2) {
  countSymbolsVector <- function(x)
    rownames(g2)[x >0] %>%
    listSymbols %>%
    length

  lapply(g2, countSymbolsVector) %>%
  unlist
}
