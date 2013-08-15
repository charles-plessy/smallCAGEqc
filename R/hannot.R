hannot <- function (annot, hierarchy="default") {

# Default hierarchy

if ( hierarchy == "default") {
  hierarchy <- data.frame(
    rbind(
    c("None",            "None"),
    c("snoscan",         "other"),
    c("antisense",       "antisense"),
    c("intron,sense\\|", "intron"),
    c("exon,sense\\|",   "exon"),
    c("lncRNA\\|",       "exon"),
    c("miRNA\\|",        "miRNA"),
    c("snRNA\\|",        "snRNA"),
    c("snoRNA\\|",       "snoRNA"),
    c("tRNA\\|",         "tRNA")),
    stringsAsFactors=FALSE
  )
}

hannot <- rep("other", length(annot))
for (i in 1:nrow(hierarchy))
  hannot[grepl(hierarchy[i,1], annot)] <- hierarchy[i,2]

factor(hannot, levels=unique(hierarchy[[2]]))
}
