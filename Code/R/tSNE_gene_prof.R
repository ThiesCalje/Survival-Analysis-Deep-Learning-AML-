##RNAseq classifier

library(tsne)


C381<-read.delim("C:/Users/thies/Downloads/PMABM000HGC_PMCRZ010BLU_RNA-Seq.gene_id.exon.counts.txt/Cohort436_Classifier381Genes20191213_annot.txt")
C381<-C381[,!names(C381)=="SJAMLM7011235_D1"]

#this is the annotation data
data2<-read.delim("C:/Users/thies/Downloads/PMABM000HGC_PMCRZ010BLU_RNA-Seq.gene_id.exon.counts.txt/coclusteringfile1.txt")

#this is the color data
COL<-read.delim("C:/Users/thies/Downloads/PMABM000HGC_PMCRZ010BLU_RNA-Seq.gene_id.exon.counts.txt/coclusteringfile3.txt")

#this is a to-be-tested sample
J<-read.delim("~/Dropbox/NKD local/miniRetreat/ETPALL/TARGET/mini2/Remove2/controlcohort/coclustering/Judith/PMABM000HGC_PMCRZ010BLU_RNA-Seq.gene_id.exon.counts.txt", skip=3)


J11<-merge(C381[,1:436],J[,c(11,4)], by.x=436, by.y=1)

sum(duplicated(J11[,1]))
#[1] 1

J11<-J11[!duplicated(J11[,1]),]

J11<-J11[,2:437]

dim(J11)
#[1] 377 437

rownames(J11)<-J11[,1]

D<-as.dist(1-cor(J11, method="spearman"))

for(i in 1:5){
  
  
  tsne.coord11<-tsne(D, max_iter=1000, perplexity=10, epoch=200)
  
  tsne.coord11<-merge(tsne.coord11, data2[,c(1,2,7)], by.x=0, by.y=1, all.x=TRUE)
  tsne.coord11<-merge(tsne.coord11, COL, by.x=5, by.y=3, all.x=TRUE)
  plot(tsne.coord11$V1, tsne.coord11$V2, pch=1, xlab="t-SNE coordinate 1", ylab="t-SNE coordinate 2", main=paste("Judith sample repeat",i), cex=1)
  points(tsne.coord11$V1[1:435], tsne.coord11$V2[1:435], pch=20, col=as.character(tsne.coord11$color[1:435]))
  points(tsne.coord11$V1[436], tsne.coord11$V2[436], pch=8, col="red", cex=2)
  
}




