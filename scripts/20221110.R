library(data.table)
library(knitr)
library(limma)
library(minfi)
library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
library(IlluminaHumanMethylation450kmanifest)
library(RColorBrewer)
library(missMethyl)
library(minfiData)
library(Gviz)
library(DMRcate)
library(stringr)
library(lumi)
library(impute)
library(data.table)



rownames(thca_methy) <- thca_methy$sample
thca_methy <- thca_methy[, -1]
table(substr(colnames(thca_methy), 14, 17))


library(impute)
set.seed(12345)
thca_methy <- impute.knn(as.matrix(thca_methy))
thca_methy_knn <- thca_methy$data
thca_methy_knn[1:5, 1:5]


ann450k <- as.data.frame(getAnnotation(IlluminaHumanMethylation450kanno.ilmn12.hg19))
ann450kSub <- ann450k[match(rownames(thca_methy_knn),ann450k$Name),
                      c(1,4,12,19,24,26)]
ann450kSub <- ann450kSub[ann450kSub$UCSC_RefGene_Name != '', ]

tmpge <- unique(unlist(strsplit(ann450kSub$UCSC_RefGene_Name[1], ';')))
length(tmpge)
paste0(tmpge, collapse = ';')

for (i in 1:nrow(ann450kSub)) {
  print(i)
  tmpge  <- unique(unlist(strsplit(ann450kSub$UCSC_RefGene_Name[i], ';')))
  if (length(tmpge) > 1) {
    tmpge <- paste0(tmpge, collapse = ';')
  }
  ann450kSub$UCSC_RefGene_Name[i] <- tmpge
}


head(ann450kSub)
tail(ann450kSub)

ann450kSub1 <- ann450kSub[str_split_fixed(ann450kSub$UCSC_RefGene_Name, ';', 2)[, 2] == '', ]
table(ann450kSub1$chr)
head(ann450kSub1)
ann450kSub1 <- subset(ann450kSub1,
                      chr != 'chrX' & chr != 'chrY')
table(ann450kSub1$chr)
# write.table(ann450kSub , 
#             file = 'files/methy_anno.txt', 
#             sep = '\t', 
#             row.names = F, 
#             quote = F)

genes

ann450kSub1.RSgenes <- ann450kSub1[ann450kSub1$UCSC_RefGene_Name %in% genes, ]
table(ann450kSub1.RSgenes$UCSC_RefGene_Name)




intersect(tcga.subtype.cli$Samples, colnames(thca_methy_knn))

thca_methy_cli <- tcga.subtype.cli[intersect(tcga.subtype.cli$Samples, colnames(thca_methy_knn)), ]
thca_methy_knn <- thca_methy_knn[, thca_methy_cli$Samples]
boxplot(thca_methy_knn[, 1:5])


dim(thca_methy_knn)
dim(thca_methy_cli)

library(knitr)
library(limma)
library(minfi)
library(IlluminaHumanMethylation450kanno.ilmn12.hg19)
library(IlluminaHumanMethylation450kmanifest)
library(RColorBrewer)
library(missMethyl)
library(minfiData)
library(Gviz)
library(DMRcate)
library(stringr)
library(lumi)

library(ChAMP)

?champ.DMP
my_DMP <- champ.DMP(beta = thca_methy_knn[, thca_methy_cli$Samples],
                    pheno = thca_methy_cli$RiskType,
                    adjPVal = 1)

head(my_DMP$Low_to_High)

my_DMP_filtered <- as.data.frame(my_DMP$Low_to_High)


my_DMP_filtered <- my_DMP_filtered[my_DMP_filtered$gene != '', ]
my_DMP_filtered1 <- my_DMP_filtered[my_DMP_filtered$adj.P.Val < 0.05, ]
my_DMP_filtered1 <- crbind2DataFrame(my_DMP_filtered1)
unique(my_DMP_filtered1$gene)




my_DMP_filtered <- my_DMP_filtered[ann450kSub1.RSgenes$Name, ]
my_DMP_filtered <- crbind2DataFrame(my_DMP_filtered)
fivenum(my_DMP_filtered$logFC)
table(my_DMP_filtered$gene)
write.csv(my_DMP_filtered,
          file = '售后/002/my_DMP_filtered.csv',
          quote = F)

table(my_DMP_filtered$P.Value < 0.05)

my_DMP_filtered$Sign <- ifelse(my_DMP_filtered$P.Value < 0.05, '*', '')
my_DMP_filtered$Sign <- paste0(rownames(my_DMP_filtered), ' ', my_DMP_filtered$Sign)


# 热图 绘制 ###########
library(ComplexHeatmap)
library(dplyr)
class(thca_methy_cli)
colnames(thca_methy_cli)
rownames(thca_methy_cli) <- thca_methy_cli$Samples
thca_methy_cli <- arrange(thca_methy_cli, RiskType)
rownames(thca_methy_cli) <- thca_methy_cli$Samples
tcga_methy_pheatmap <- t(scale(t(thca_methy_knn[ann450kSub1.RSgenes$Name, 
                                                 thca_methy_cli$Samples])))

head(ann450kSub1.RSgenes)
ann450kSub1.RSgenes <- cbind(ann450kSub1.RSgenes,
                             my_DMP_filtered[ann450kSub1.RSgenes$Name, c("P.Value", "Sign")])
head(ann450kSub1.RSgenes)

rownames(tcga_methy_pheatmap) <- ann450kSub1.RSgenes$Sign

table(thca_methy_cli$RiskType)
tcga_methy_plot <- Heatmap(tcga_methy_pheatmap[ann450kSub1.RSgenes$Sign, thca_methy_cli$Samples]
                             , name = "score"
                             , col = circlize::colorRamp2(c(-2, 0, 2), c(mycolor[1], 'white', mycolor[2]))
                             , border = T
                             , show_column_names = F
                             , show_column_dend = F
                             , show_row_dend = F
                             , cluster_columns = T
                             , cluster_rows = T
                             , column_split = factor(thca_methy_cli$RiskType)
                             , row_split = factor(ann450kSub1.RSgenes$UCSC_RefGene_Name)
                             , clustering_distance_rows  ='pearson'
                             , clustering_method_rows = 'ward.D2'
                             , clustering_distance_columns  ='pearson'
                             , clustering_method_columns = 'ward.D2'
                             , row_names_gp = gpar(fontsize = 10)
                             , top_annotation = HeatmapAnnotation(RiskType = thca_methy_cli$RiskType
                                                                  , col=list(RiskType=c('High'=mycolor[1], 
                                                                                       'Low' = mycolor[2]))
                                                                  , annotation_width = unit(c(1,2), 'cm')
                                                                  , annotation_height = unit(0.2, "cm")
                                                                  , gap = unit(1, 'mm')))

tcga_methy_plot
# dir.create('售后/001', recursive = T)
dev.off()
pdf('售后/002/tcga_methy_plot.pdf',
    width = 10, height = 20)
tcga_methy_plot
dev.off()





# 基因与探针的相关性 ##############

library(psych)


gene.methy_cor <- corr.test(t(tcga_tpm_log2[genes, thca_methy_cli$Samples]),
                            t(thca_methy_knn[ann450kSub1.RSgenes$Name, thca_methy_cli$Samples]),
                            adjust = 'none',
                            ci = F)

gene.methy_cor.R <- t(gene.methy_cor$r)
gene.methy_cor.P <- t(gene.methy_cor$p)


gene_methy_cor_dat <- data.frame()
for (ge in genes) {
  print(ge)
  me <- ann450kSub1.RSgenes$Name[ann450kSub1.RSgenes$UCSC_RefGene_Name == ge]
  print(me)
  gene.methy_cor <- cbind(gene.methy_cor.R[me, ge],
                          gene.methy_cor.P[me, ge])
  colnames(gene.methy_cor) <- c('Corr', 'P')
  gene.methy_cor <- as.data.frame(gene.methy_cor)
  gene.methy_cor$Methy <- rownames(gene.methy_cor)
  gene.methy_cor$Genes <- ge 
  
  gene_methy_cor_dat <- rbind(gene_methy_cor_dat, gene.methy_cor)
}



write.csv(gene_methy_cor_dat,
          file = '售后/001/gene_methy_cor_dat.csv')




# mRNAsi 与 甲基化的相关性以及功能富集分析 ################


miRNA.methy_cor <- corr.test(as.matrix(thca_methy_cli[, "mRNAsi"]),
                            t(thca_methy_knn[ann450kSub1$Name, thca_methy_cli$Samples]),
                            adjust = 'none',
                            ci = F)

miRNA.methy_cor$r[, 1:5]
miRNA.methy.cor.res <- rbind(miRNA.methy_cor$r,
                             miRNA.methy_cor$p)
miRNA.methy.cor.res[, 1:5]
rownames(miRNA.methy.cor.res) <- c('Corr', 'P')

miRNA.methy.cor.res <- as.data.frame(t(miRNA.methy.cor.res))


miRNA.methy.cor.res.filtered <- subset(miRNA.methy.cor.res,
                                       abs(Corr) > 0.3 & P < 0.05)
head(miRNA.methy.cor.res.filtered)
write.csv(miRNA.methy.cor.res.filtered,
          file = '售后/001/miRNA.methy.cor.res.filtered.csv')


mRNAsi.methy.genes.dat <- ann450kSub1[match(rownames(miRNA.methy.cor.res.filtered), ann450kSub1$Name), ]
mRNAsi.methy.genes <- unique(mRNAsi.methy.genes.dat$UCSC_RefGene_Name)



mRNAsi_methy_go_kegg <- enrichmentORA(mRNAsi.methy.genes,
                                mp_dbs=c('pathway_KEGG',
                                         'geneontology_Biological_Process',
                                         'geneontology_Cellular_Component',
                                         'geneontology_Molecular_Function'))
mRNAsi_methy_go_kegg_filtered <- mRNAsi_methy_go_kegg[mRNAsi_methy_go_kegg$FDR < 0.05, ]
table(mRNAsi_methy_go_kegg_filtered$DB)
write.table(mRNAsi_methy_go_kegg_filtered,
            file = '售后/001/mRNAsi_methy_go_kegg_filtered.txt',
            quote = F, sep = '\t')


library(dplyr)
mRNAsi_methy_go_kegg_filtered_1 <- mRNAsi_methy_go_kegg_filtered[c(6,11,16,17,24,25,32,43,58,3,5,
                                                       106:876), ]

mRNAsi_methy_go_kegg_filtered_1.top <- get_top(data = mRNAsi_methy_go_kegg_filtered_1)
mRNAsi_methy_go_kegg_filtered_1.top

mRNAsi_methy_go_kegg_filtered_1.plot <- go_kegg_barplot(mRNAsi_methy_go_kegg_filtered_1.top,
                                                  col = mycolor[c(1,2,3,5)])
mRNAsi_methy_go_kegg_filtered_1.plot

ggsave(plot = mRNAsi_methy_go_kegg_filtered_1.plot,
       filename = '售后/001/mRNAsi_methy_go_kegg_filtered_1.plot.pdf',
       width = 8, height = 8)







