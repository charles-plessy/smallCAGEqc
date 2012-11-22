mapStats <- function(libs) {

maprates <- with(libs, data.frame(
  Group=Group,
  Promoter=(promoter / Extracted),
  Exon=(exon / Extracted),
  Intron=(intron / Extracted),
  Mapped=((Mapped  - promoter - intron - exon )/ Extracted),
  rRNA=(rRNA / Extracted),
  TagDust=(TagDust / Extracted)
))

mapstats <- with(maprates, data.frame(
  Promoter=tapply(Promoter, Group, mean),
  Exon=tapply(Exon, Group, mean),
  Intron=tapply(Intron, Group, mean),
  Mapped=tapply(Mapped, Group, mean),
  rRNA=tapply(rRNA, Group, mean),
  TagDust=tapply(TagDust, Group, mean)
))

mapstats$Type <- rownames(mapstats)

mapstats.sd <- with(maprates, data.frame(
  Promoter=tapply(Promoter, Group, sd),
  Exon=tapply(Exon, Group, sd),
  Intron=tapply(Intron, Group, sd),
  Mapped=tapply(Mapped, Group, sd),
  rRNA=tapply(rRNA, Group, sd),
  TagDust=tapply(TagDust, Group, sd)
))

mapstats.sd$Type <- rownames(mapstats.sd)

mapstats <- melt(mapstats)

mapstats$sd <- melt(mapstats.sd)$value

mapstats <- ddply(mapstats,.(Type),transform,ystart = cumsum(value),yend = cumsum(value) + sd)

return(mapstats)

}
