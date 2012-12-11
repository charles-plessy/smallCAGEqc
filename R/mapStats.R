mapStats <- function(libs, scope='all') {

if (scope == 'all')
  if (is.numeric(libs$extracted))
    scope <- libs$extracted
  else
    stop("libs$extracted missing or erroneous.")
else if (scope == 'annotation')
  if (is.numeric(libs$mapped))
    scope <- libs$mapped
  else
    stop("libs$mapped missing or erroneous.")
else
  stop ('scope must be "all" or "annotation"')

maprates <- with(libs, data.frame(
  group=group,
  promoter=(promoter / scope),
  exon=(exon / scope),
  intron=(intron / scope),
  mapped=((mapped  - promoter - intron - exon )/ scope),
  rdna=(rdna / scope),
  tagdust=(tagdust / scope)
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
