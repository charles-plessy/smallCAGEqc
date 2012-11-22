hierarchAnnot <- function (annot) {
  annot$class <- 'other'
  annot[grep("\\.",        annot$feature), "class"] <- 'unknown'
  annot[grep("gene",       annot$feature), "class"] <- 'intron' # Because exons are overridden at next command.
  annot[grep("exon",       annot$feature), "class"] <- 'exon'
  annot[grep("promoter",   annot$feature), "class"] <- 'promoter' 
  annot$class <- factor(annot$class)
  annot
}
