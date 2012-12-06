mapStats <- function(libs) {

maprates <- with(libs, data.frame(
  group=group,
  promoter=(promoter / extracted),
  exon=(exon / extracted),
  intron=(intron / extracted),
  mapped=((mapped  - promoter - intron - exon )/ extracted),
  rdna=(rdna / extracted),
  tagdust=(tagdust / extracted)
))

mapstats <- with(maprates, data.frame(
  promoter=tapply(promoter, group, mean),
  exon=tapply(exon, group, mean),
  intron=tapply(intron, group, mean),
  mapped=tapply(mapped, group, mean),
  rdna=tapply(rdna, group, mean),
  tagdust=tapply(tagdust, group, mean)
))

mapstats$group <- rownames(mapstats)

mapstats.sd <- with(maprates, data.frame(
  promoter=tapply(promoter, group, sd),
  exon=tapply(exon, group, sd),
  intron=tapply(intron, group, sd),
  mapped=tapply(mapped, group, sd),
  rdna=tapply(rdna, group, sd),
  tagdust=tapply(tagdust, group, sd)
))

mapstats.sd$group <- rownames(mapstats.sd)

mapstats <- melt(mapstats)

mapstats$sd <- melt(mapstats.sd)$value

mapstats <- ddply(mapstats,.(group),transform,ystart = cumsum(value),yend = cumsum(value) + sd)

return(mapstats)

}
