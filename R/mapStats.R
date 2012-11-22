mapStats <- function(libs) {

maprates <- with(libs, data.frame(
  Promoter=(promoter / Extracted),
  Exon=(exon / Extracted),
  Intron=(intron / Extracted),
  Mapped=((Mapped  - promoter - intron - exon )/ Extracted),
  rRNA=(rRNA / Extracted),
  TagDust=(TagDust / Extracted)
))

mapstats <- with(maprates, data.frame(
  Promoter=tapply(Promoter, Type, mean),
  Exon=tapply(Exon, Type, mean),
  Intron=tapply(Intron, Type, mean),
  Mapped=tapply(Mapped, Type, mean),
  rRNA=tapply(rRNA, Type, mean),
  TagDust=tapply(TagDust, Type, mean)
))

mapstats$Type <- rownames(mapstats)

mapstats.sd <- with(maprates, data.frame(
  Promoter=tapply(Promoter, Type, sd),
  Exon=tapply(Exon, Type, sd),
  Intron=tapply(Intron, Type, sd),
  Mapped=tapply(Mapped, Type, sd),
  rRNA=tapply(rRNA, Type, sd),
  TagDust=tapply(TagDust, Type, sd)
))

mapstats.sd$Type <- rownames(mapstats.sd)

return(cbind(mapstats, mapstats.sd))
}
