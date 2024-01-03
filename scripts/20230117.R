
library(survival)
tmp.OS <- ggplotKMCox(data.frame(tcga_mut_cli$OS.time / 365,
                       tcga_mut_cli$OS,
                       as.character(tcga_mut_cli[, "BRAF"])),
            title = 'BRAF')

tmp.PFI <- ggplotKMCox(data.frame(tcga_mut_cli$PFI.time / 365,
                       tcga_mut_cli$PFI,
                       as.character(tcga_mut_cli[, "BRAF"])),
            title = 'BRAF')


tmp.DFI <- ggplotKMCox(data.frame(tcga_mut_cli$DFI.time / 365,
                       tcga_mut_cli$DFI,
                       as.character(tcga_mut_cli[, "BRAF"])),
            title = 'BRAF')


tmp.DSS <- ggplotKMCox(data.frame(tcga_mut_cli$DSS.time / 365,
                       tcga_mut_cli$DSS,
                       as.character(tcga_mut_cli[, "BRAF"])),
            title = 'BRAF')

OS.snv.genes.filter.km.list <- list()
PFI.snv.genes.filter.km.list <- list()
DFI.snv.genes.filter.km.list <- list()
DSS.snv.genes.filter.km.list <- list()
for (ge in snv.genes.filter) {
  print(ge)
  tmp.OS <- ggplotKMCox(data.frame(tcga_mut_cli$OS.time / 365,
                                   tcga_mut_cli$OS,
                                   as.character(tcga_mut_cli[, ge])),
                        title = ge)
  OS.snv.genes.filter.km.list[[ge]] <- tmp.OS
  
  tmp.PFI <- ggplotKMCox(data.frame(tcga_mut_cli$PFI.time / 365,
                                    tcga_mut_cli$PFI,
                                    as.character(tcga_mut_cli[, ge])),
                         title = ge)
  PFI.snv.genes.filter.km.list[[ge]] <- tmp.PFI
  
  
  tmp.DFI <- ggplotKMCox(data.frame(tcga_mut_cli$DFI.time / 365,
                                    tcga_mut_cli$DFI,
                                    as.character(tcga_mut_cli[, ge])),
                         title = ge)
  DFI.snv.genes.filter.km.list[[ge]] <- tmp.DFI
  
  
  tmp.DSS <- ggplotKMCox(data.frame(tcga_mut_cli$DSS.time / 365,
                                    tcga_mut_cli$DSS,
                                    as.character(tcga_mut_cli[, ge])),
                         title = ge)
  DSS.snv.genes.filter.km.list[[ge]] <- tmp.DSS
}


OS.snv.genes.filter.km <- cowplot::plot_grid(plotlist = OS.snv.genes.filter.km.list,
                                             ncol = 4)
OS.snv.genes.filter.km


PFI.snv.genes.filter.km <- cowplot::plot_grid(plotlist = PFI.snv.genes.filter.km.list,
                                             ncol = 4)
PFI.snv.genes.filter.km



DFI.snv.genes.filter.km <- cowplot::plot_grid(plotlist = DFI.snv.genes.filter.km.list,
                                              ncol = 4)
DFI.snv.genes.filter.km


DSS.snv.genes.filter.km <- cowplot::plot_grid(plotlist = DSS.snv.genes.filter.km.list,
                                              ncol = 4)
DSS.snv.genes.filter.km




                     check.names = F, stringsAsFactors = F)

# GSE33630 ###########
GSE33630 <- getGEOExpData('GSE33630')
GSE33630_cli <- GSE33630$Sample
table(GSE33630_cli$Source)
GSE33630_cli <- GSE33630_cli[GSE33630_cli$Source == 'tumour', ]

GSE33630_exp <- GSE33630$Exp$GPL570_54675_Data_col1
GSE33630_exp <- exp_probe2symbol_v2(datExpr = GSE33630_exp,
                                    anno = gpl570[, c(5, 3)])
boxplot(GSE33630_exp[, 11:5])
GSE33630_exp <- as.data.frame(GSE33630_exp)
GSE33630_exp <- GSE33630_exp[, GSE33630_cli$Acc]
save(GSE33630_exp,
     file = 'GSE33630_exp.Rdata')
load('GSE33630_exp.Rdata')


GSE33630_mRNAsi <- calculate_mRNAsi(exp_file = GSE33630_exp)
save(GSE33630_mRNAsi,
     file = 'GSE33630_mRNAsi.Rdata')
load('GSE33630_mRNAsi.Rdata')
GSE33630_mRNAsi_res <- GSE33630_mRNAsi$mRNAsi_index


GSE33630_estimate <- immu_estimate(GSE33630_exp, isTCGA = F, platform='affymetrix')
class(GSE33630_estimate)
head(GSE33630_estimate)


GSE33630_estimate1 <- as.data.frame(GSE33630_estimate)
GSE33630_estimate1$samples <- rownames(GSE33630_estimate1)
GSE33630_estimate1 <- merge(GSE33630_estimate1, GSE33630_mRNAsi_res, by.x = 'samples', by.y = 'samples')

GSE33630_estimate_mRNAsi <- ggpubr::ggarrange(cor_point(GSE33630_estimate1$StromalScore,
                                                    GSE33630_estimate1$mRNAsi,
                                                    method='Pearson',
                                                    top_col='red',
                                                    right_col='blue',
                                                    ylab='mRNAsi',
                                                    xlab='StromalScore'),
                                          cor_point(GSE33630_estimate1$ImmuneScore,
                                                    GSE33630_estimate1$mRNAsi,
                                                    method='Pearson',
                                                    top_col='red',
                                                    right_col='blue',
                                                    ylab='mRNAsi',
                                                    xlab='ImmuneScore'),
                                          cor_point(GSE33630_estimate1$ESTIMATEScore,
                                                    GSE33630_estimate1$mRNAsi,
                                                    method='Pearson',
                                                    top_col='red',
                                                    right_col='blue',
                                                    ylab='mRNAsi',
                                                    xlab='ESTIMATEScore'),
                                          ncol = 3,nrow = 1,
                                          labels = toupper(letters)[1:3],align = "hv")
GSE33630_estimate_mRNAsi
ggsave(plot = GSE33630_estimate_mRNAsi,
       filename = '03_mRNAsi_immune/GSE33630_estimate_mRNAsi.pdf',
       width = 15, height = 5, device = cairo_pdf)


# GSE3467 ###########
GSE3467 <- getGEOExpData('GSE3467')
GSE3467_cli <- GSE3467$Sample


GSE3467_exp <- GSE3467$Exp$GPL570_54675_Data_col1
GSE3467_exp <- exp_probe2symbol_v2(datExpr = GSE3467_exp,
                                    anno = gpl570[, c(5, 3)])
boxplot(GSE3467_exp[, 11:5])
GSE3467_exp <- as.data.frame(GSE3467_exp)
GSE3467_exp <- GSE3467_exp[, GSE3467_cli$Acc]
save(GSE3467_exp,
     file = 'GSE3467_exp.Rdata')
load('GSE3467_exp.Rdata')


GSE3467_mRNAsi <- calculate_mRNAsi(exp_file = GSE3467_exp)
save(GSE3467_mRNAsi,
     file = 'GSE3467_mRNAsi.Rdata')
load('GSE3467_mRNAsi.Rdata')
GSE3467_mRNAsi_res <- GSE3467_mRNAsi$mRNAsi_index


GSE3467_estimate <- immu_estimate(GSE3467_exp, isTCGA = F, platform='affymetrix')
class(GSE3467_estimate)
head(GSE3467_estimate)


GSE3467_estimate1 <- as.data.frame(GSE3467_estimate)
GSE3467_estimate1$samples <- rownames(GSE3467_estimate1)
GSE3467_estimate1 <- merge(GSE3467_estimate1, GSE3467_mRNAsi_res, by.x = 'samples', by.y = 'samples')

GSE3467_estimate_mRNAsi <- ggpubr::ggarrange(cor_point(GSE3467_estimate1$StromalScore,
                                                        GSE3467_estimate1$mRNAsi,
                                                        method='Pearson',
                                                        top_col='red',
                                                        right_col='blue',
                                                        ylab='mRNAsi',
                                                        xlab='StromalScore'),
                                              cor_point(GSE3467_estimate1$ImmuneScore,
                                                        GSE3467_estimate1$mRNAsi,
                                                        method='Pearson',
                                                        top_col='red',
                                                        right_col='blue',
                                                        ylab='mRNAsi',
                                                        xlab='ImmuneScore'),
                                              cor_point(GSE3467_estimate1$ESTIMATEScore,
                                                        GSE3467_estimate1$mRNAsi,
                                                        method='Pearson',
                                                        top_col='red',
                                                        right_col='blue',
                                                        ylab='mRNAsi',
                                                        xlab='ESTIMATEScore'),
                                              ncol = 3,nrow = 1,
                                              labels = toupper(letters)[1:3],align = "hv")
GSE3467_estimate_mRNAsi
ggsave(plot = GSE3467_estimate_mRNAsi,
       filename = '售后/004/GSE3467_estimate_mRNAsi.pdf',
       width = 15, height = 5, device = cairo_pdf)



GSE3467_ssgsea <- immu_ssgsea(exp = GSE3467_exp, isTCGA = F)
class(GSE3467_ssgsea)
GSE3467_ssgsea <- as.data.frame(GSE3467_ssgsea)

GSE3467_ssgsea1 <- cbind(GSE3467_estimate1[, c("samples", "mRNAsi")],
                      GSE3467_ssgsea[GSE3467_estimate1$samples, ])


GSE3467_ssgsea_mRNAsi_cor_list <- list()
for (ce in colnames(GSE3467_ssgsea)) {
  print(ce)
  
  tmp.dat <- GSE3467_ssgsea1[, c('mRNAsi', ce)]
  colnames(tmp.dat) <- c('mRNAsi', 'Cell')
  
  tmp <- cor_point(as.numeric(tmp.dat[, 'Cell']),
                   as.numeric(tmp.dat[, 'mRNAsi']),
                   method='Pearson',
                   top_col='red',
                   right_col='blue',
                   ylab= 'mRNAsi',
                   xlab= ce)
  GSE3467_ssgsea_mRNAsi_cor_list[[ce]] <- tmp
}

GSE3467_ssgsea_mRNAsi_cor_plot <- cowplot::plot_grid(plotlist = GSE3467_ssgsea_mRNAsi_cor_list,
                                                  ncol = 5)
GSE3467_ssgsea_mRNAsi_cor_plot
ggsave(plot = GSE3467_ssgsea_mRNAsi_cor_plot,
       filename = '售后/004/GSE3467_ssgsea_mRNAsi_cor_plot.pdf',
       width = 25, height = 25, device = cairo_pdf)



library(psych)
GSE3467.cor.dat=GSE3467_exp[rownames(GSE3467_exp) %in% genes_protein, GSE3467_estimate1$samples]
dim(GSE3467.cor.dat)
GSE3467.cor.dat <- GSE3467.cor.dat[rowSums(GSE3467.cor.dat) > 0, ]


GSE3467_mRNAsi_gene_cor <- corr.test(t(GSE3467.cor.dat),
                                  as.matrix(GSE3467_estimate1[, "mRNAsi"]))

GSE3467_mRNAsi_gene_cor$r

GSE3467_mRNAsi_gene_cor_res <- cbind(GSE3467_mRNAsi_gene_cor$r,
                                  GSE3467_mRNAsi_gene_cor$p)
colnames(GSE3467_mRNAsi_gene_cor_res) <- c('Corr', 'P')
GSE3467_mRNAsi_gene_cor_res <- as.data.frame(GSE3467_mRNAsi_gene_cor_res)
table(GSE3467_mRNAsi_gene_cor_res$Corr > 0.2, GSE3467_mRNAsi_gene_cor_res$P < 0.05)
table(GSE3467_mRNAsi_gene_cor_res$Corr < -0.3, GSE3467_mRNAsi_gene_cor_res$P < 0.05)
table(abs(GSE3467_mRNAsi_gene_cor_res$Corr) > 0.4, GSE3467_mRNAsi_gene_cor_res$P < 0.05)


GSE3467_mRNAsi_gene_cor_filtered <- GSE3467_mRNAsi_gene_cor_res[abs(GSE3467_mRNAsi_gene_cor_res$Corr) > 0.4 & GSE3467_mRNAsi_gene_cor_res$P < 0.05, ]
dim(GSE3467_mRNAsi_gene_cor_filtered)
head(GSE3467_mRNAsi_gene_cor_filtered)
write.csv(GSE3467_mRNAsi_gene_cor_filtered,
          file = '售后/004/GSE3467_mRNAsi_gene_cor_filtered.csv',
          quote = F)


GSE3467_mRNAsi_genes <- rownames(GSE3467_mRNAsi_gene_cor_res)[abs(GSE3467_mRNAsi_gene_cor_res$Corr) > 0.4 & GSE3467_mRNAsi_gene_cor_res$P < 0.05]


intersect(GSE3467_mRNAsi_genes, mRNAsi_genes)
pdf('售后/004/venn.pdf', width = 5, height = 5)
mg_venn_plot(data_list = list(GSE3467 = GSE3467_mRNAsi_genes,
                              TCGA = mRNAsi_genes))
dev.off()


# 功能富集分析 ######################
GSE3467_mRNAsi_go_kegg <- enrichmentORA(GSE3467_mRNAsi_genes,
                                mp_dbs=c('pathway_KEGG',
                                         'geneontology_Biological_Process',
                                         'geneontology_Cellular_Component',
                                         'geneontology_Molecular_Function'))
GSE3467_mRNAsi_go_kegg_filtered <- GSE3467_mRNAsi_go_kegg[GSE3467_mRNAsi_go_kegg$FDR < 0.05, ]
table(GSE3467_mRNAsi_go_kegg_filtered$DB)
write.table(GSE3467_mRNAsi_go_kegg_filtered,
            file = '售后/004/GSE3467_mRNAsi_go_kegg_filtered.txt',
            quote = F, sep = '\t')


library(dplyr)
GSE3467_mRNAsi_go_kegg_filtered_1 <- GSE3467_mRNAsi_go_kegg_filtered
GSE3467_mRNAsi_go_kegg_filtered_1.top <- get_top(data = GSE3467_mRNAsi_go_kegg_filtered_1)
GSE3467_mRNAsi_go_kegg_filtered_1.top

GSE3467_mRNAsi_go_kegg_filtered_1.plot <- go_kegg_barplot(GSE3467_mRNAsi_go_kegg_filtered_1.top,
                                                  col = mycolor[c(1,2,3,5)])
GSE3467_mRNAsi_go_kegg_filtered_1.plot

ggsave(plot = GSE3467_mRNAsi_go_kegg_filtered_1.plot,
       filename = '售后/004/GSE3467_mRNAsi_go_kegg_filtered_1.plot.pdf',
       width = 8, height = 8)




