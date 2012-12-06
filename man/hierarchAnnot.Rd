\name{hierarchAnnot}
\alias{hierarchAnnot}
\title{Hierarchical annotation of the CAGE clusters}
\description{
Parse an annotation data frame and retains only one annotation,
in the order \sQuote{promoter}, \sQuote{exon}, \sQuote{intron} and \sQuote{unknown}.
}
\usage{
hierarchAnnot(annot)
}
\arguments{
  \item{annot}{
A data frame with a \code{feature} column that contains comma-separated annotations,
like for instance \sQuote{exon,intron}.
}
}
\details{
The annotation data frame is typically loaded from a file produces with
commands such as the following.

\preformatted{
zcat /analysisdata/annotations/100712hg19/refGeneTssProximal500.bed.gz | awk '{OFS="\t"}{print $1, $2, $3, "promoter", $5, $6}' >  100712hg19.annot
zcat /analysisdata/annotations/100712hg19/refGeneExon.bed.gz |           awk '{OFS="\t"}{print $1, $2, $3, "exon",     $5, $6}' >> 100712hg19.annot
zcat /analysisdata/annotations/100712hg19/refGene.bed.gz |               awk '{OFS="\t"}{print $1, $2, $3, "gene",     $5, $6}' >> 100712hg19.annot

zcat $LIBRARY.l1.gz | grep -v \# | sed 1d | awk '{OFS="\t"}{print $2, $3, $4, "l2", "1000", $5}' > $LIBRARY.l1.bed
zcat $LIBRARY.l2.gz | grep -v \# | sed 1d | awk '{OFS="\t"}{print $2, $3, $4, "l2", "1000", $5}' > $LIBRARY.l2.bed

bedtools intersect -a $LIBRARY.l1.bed -b 100712hg19.annot -s -loj |
  awk '{OFS="\t"}{print $1":"$2"-"$3$6,$10}' | 
  bedtools groupby -g 1 -c 2 -o collapse > $LIBRARY.l1.annot

bedtools intersect -a $LIBRARY.l2.bed -b 100712hg19.annot -s -loj |
  awk '{OFS="\t"}{print $1":"$2"-"$3$6,$10}' | 
  bedtools groupby -g 1 -c 2 -o collapse > $LIBRARY.l2.annot
}
}

\value{
Returns the annotation table with the hierarchical annotation in a column called \code{class}.
}
\author{
Charles Plessy <plessy@riken.jp>
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.
}

\keyword{ ~CAGE }
\keyword{ ~annotation }% __ONLY ONE__ keyword per line
