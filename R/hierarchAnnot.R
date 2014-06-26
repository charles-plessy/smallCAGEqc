hierarchAnnot <- function (annot) {
  annot$class <- 'other'
  annot[grepl("\\.",        annot$feature), "class"] <- 'unknown'
  annot[grepl("antisense",  annot$feature), "class"] <- 'antisense'
  annot[grepl("gene",       annot$feature), "class"] <- 'intron' # Because exons are overridden at next command.
  annot[grepl("exon",       annot$feature), "class"] <- 'exon'
  annot[grepl("promoter",   annot$feature), "class"] <- 'promoter' 
  annot$class <- factor(annot$class)
  annot
}
