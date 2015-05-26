listSymbols <- function(...) {
  c(...) %>%
  subset(subset=( . != '.')) %>%
  paste(collapse=',') %>%
  read.table(text=., colClasses="character", sep=',') %>%
  unlist %>%
  unique
}
