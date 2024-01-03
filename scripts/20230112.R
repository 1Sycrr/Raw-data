coxFun <- function(dat){
  library(survival)
  colnames(dat)=c('time','status','gene')
  fmla=as.formula("Surv(time,status)~gene")
  cox=coxph(fmla,data=dat)
  p=summary(cox)[[7]][5]
  result=c(p,summary(cox)[[8]][1],summary(cox)[[8]][3],summary(cox)[[8]][4])
  return(result)
}

tcga_mRNAsi_OS_cox <- coxFun(dat = na.omit(tcga_cli[, c("OS.time", "OS", "mRNAsi")]))
tcga_mRNAsi_PFI_cox <- coxFun(dat = na.omit(tcga_cli[, c("PFI.time", "PFI", "mRNAsi")]))
tcga_mRNAsi_DFI_cox <- coxFun(dat = na.omit(tcga_cli[, c("DFI.time", "DFI", "mRNAsi")]))
tcga_mRNAsi_DSS_cox <- coxFun(dat = na.omit(tcga_cli[, c("DSS.time", "DSS", "mRNAsi")]))


tcga_mRNAsi_cox <- rbind(tcga_mRNAsi_OS_cox,
                         tcga_mRNAsi_PFI_cox,
                         tcga_mRNAsi_DFI_cox,
                         tcga_mRNAsi_DSS_cox)
tcga_mRNAsi_cox
colnames(tcga_mRNAsi_cox) <- c('p.value','HR','Low 95%CI','High 95%CI')
tcga_mRNAsi_cox <- as.data.frame(tcga_mRNAsi_cox)
library(stringr)
rownames(tcga_mRNAsi_cox) <- str_split_fixed(rownames(tcga_mRNAsi_cox), '_', 4)[, 3]
write.table(tcga_mRNAsi_cox,
            file = '售后/003/tcga_mRNAsi_cox.txt',
            quote = F, sep = '\t')



# 肿瘤驱动基因 #####
cancer_driver_genes <- openxlsx::read.xlsx('00_origin_datas/cancer.driver.genes.PMC4160307.xlsx', sheet = 1)[, 1]

dim(tcga_tpm_log2)

intersect(cancer_driver_genes, rownames(tcga_tpm_log2))


cor_point(as.numeric(tcga_tpm_log2["PIK3CA", tcga_cli$Samples]),
          tcga_cli$mRNAsi,
          method='Pearson',
          top_col='red',
          right_col='blue',
          ylab='mRNAsi',
          xlab="PIK3CA")




dir.create('售后/003/Corr')
for (ge in intersect(cancer_driver_genes, rownames(tcga_tpm_log2))) {
  print(ge)
  # tmp <- data.frame(mRNAsi = tcga_cli$mRNAsi,
  #                   Gene = as.numeric(tcga_tpm_log2[ge, tcga_cli$Samples]))
  
  tmp.plot <- cor_point(as.numeric(tcga_tpm_log2[ge, tcga_cli$Samples]),
                        tcga_cli$mRNAsi,
                        method='Pearson',
                        top_col='red',
                        right_col='blue',
                        ylab='mRNAsi',
                        xlab=ge)
  ggsave(plot = tmp.plot,
         filename = paste0('售后/003/Corr/', ge, '.pdf'),
         width = 7, height = 5)
  
}

library(psych)
tcga_mRNAsi_gene_cor <- corr.test(t(tcga_tpm_log2[intersect(cancer_driver_genes, 
                                                            rownames(tcga_tpm_log2)), 
                                                  tcga_cli$Samples]),
                                  tcga_cli[, c("Age", "mRNAsi")],
                                  ci = F, 
                                  adjust = 'none')


tcga_mRNAsi_gene_cor <- cbind(tcga_mRNAsi_gene_cor$r[, "mRNAsi"],
                              tcga_mRNAsi_gene_cor$p[, "mRNAsi"])
colnames(tcga_mRNAsi_gene_cor) <- c("Corr", 'P')
head(tcga_mRNAsi_gene_cor)
tcga_mRNAsi_gene_cor <- as.data.frame(tcga_mRNAsi_gene_cor)
table(tcga_mRNAsi_gene_cor$P < 0.05)

write.csv(tcga_mRNAsi_gene_cor,
          file = '售后/003/tcga_mRNAsi_gene_cor.csv',
          quote = F)
















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
library(openxlsx)
write.xlsx(x = my_DMP_filtered1,
           file = '售后/003/my_DMP_filtered.xlsx',
           overwrite = T, row.names = T)
unique(my_DMP_filtered1$gene)



my_DMP_go_kegg <- enrichmentORA(unique(my_DMP_filtered1$gene),
                                mp_dbs=c('pathway_KEGG',
                                         'geneontology_Biological_Process',
                                         'geneontology_Cellular_Component',
                                         'geneontology_Molecular_Function'))
my_DMP_go_kegg_filtered <- my_DMP_go_kegg[my_DMP_go_kegg$FDR < 0.05, ]
table(my_DMP_go_kegg_filtered$DB)
write.table(my_DMP_go_kegg_filtered,
            file = '售后/003/my_DMP_go_kegg_filtered.txt',
            quote = F, sep = '\t')


library(dplyr)
my_DMP_go_kegg_filtered_1 <- my_DMP_go_kegg_filtered[c(8,10,18,25,27,30,40,50,51,55,61,
                                                       68:1418), ]

my_DMP_go_kegg_filtered_1.top <- get_top(data = my_DMP_go_kegg_filtered_1)
my_DMP_go_kegg_filtered_1.top

my_DMP_go_kegg_filtered_1.plot <- go_kegg_barplot(my_DMP_go_kegg_filtered_1.top,
                                                  col = mycolor[c(1,2,3,5)])
my_DMP_go_kegg_filtered_1.plot

ggsave(plot = my_DMP_go_kegg_filtered_1.plot,
       filename = '售后/003/my_DMP_go_kegg_filtered_1.plot.pdf',
       width = 8, height = 8)













