rm(list = ls());gc()
library(dplyr);library(RColorBrewer)
library(ggsci);library(scales)
library(ggpubr)
library(ggrepel)

#########################################################
# 函数 plot_Volcano_1
#########################################################
plot_Volcano_1 <- function(result, logFC = 1.5, adj_P = 0.1, label_geneset = NULL) {
  result$Type = "NONE"
  result$Type[which(result$log2FoldChange > logFC & result$padj < adj_P)] = "UP"
  result$Type[which(result$log2FoldChange < (-logFC) & result$padj < adj_P)] = "DOWN"
  xlim = max(abs(result$log2FoldChange))
  if(is.null(label_geneset)) {
    p = ggplot(result, aes(x = log2FoldChange, y = -log10(padj)))+
      geom_point(data = result, aes(x = log2FoldChange, y = -log10(padj), color = Type)) +
      theme_bw() +
      geom_vline(xintercept = c(-logFC,logFC), lty = 2)+
      geom_hline(yintercept = c(-log10(adj_P)), lty = 2)+
      scale_x_continuous(limits = c(-xlim, xlim))+
      coord_fixed(ratio = ( 2*xlim )/(max(-log10(result$padj), na.rm = T)) )+
      theme(panel.grid = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(color = "black"))+
      xlab("log10FoldChange")+ylab("-log10P-value")+
      scale_color_manual(values = c("NONE" = "grey","UP" = "red","DOWN" = "blue"))
  } else {
    p = ggplot(result,aes(x = log2FoldChange, y = -log10(padj)))+
      geom_point(data = result, 
                 aes(x = log2FoldChange, y = -log10(padj), color = Type), alpha=0.9)+
      geom_point(data = result[which(result$Row.names %in% label_geneset),],
                 aes(x = log2FoldChange, y = -log10(padj)),color = "black",size = 4)+
      geom_point(data = result[which(result$Row.names %in% label_geneset),],
                 aes(x = log2FoldChange, y = -log10(padj)),color = "white",size = 2.5)+
      geom_point(data = result[which(result$Row.names %in% label_geneset),],
                 aes(x = log2FoldChange, y = -log10(padj),color = Type),size = 1.5)+
      geom_text_repel(data = result[which(result$Row.names %in% label_geneset),],
                      aes(x = log2FoldChange, y = -log10(padj), label = Row.names), 
                      fontface = "bold")+ #fontface = "italic"
      theme_bw() +
      geom_vline(xintercept = c(-logFC,logFC),lty = 2)+
      geom_hline(yintercept = c(-log10(adj_P)),lty = 2)+
      scale_x_continuous(limits = c(-xlim,xlim))+
      coord_fixed(ratio = ( 2*xlim )/(max(-log10(result$padj),na.rm = T))  )+
      theme(panel.grid = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(color = "black"))+
      xlab("log10FoldChange")+ylab("-log10P-value")+
      scale_color_manual(values = c("UP" = jco[8],"DOWN" = jco[4],"NONE" = "grey"))
  }
  return(p)
}

# 
if (filterc == 'fppadj') {
  df_valcano$sign <- ifelse(df_valcano$padj < ffdr & df_valcano$pvalue < fpval, 
                            ifelse(abs(df_valcano$log2FoldChange) > flogfc, 
                                   ifelse(df_valcano$log2FoldChange > flogfc, 'Up', 'Down'), 
                                   'Not'),
                            'Not')
} else if (filterc == 'fpadj') {
  df_valcano$sign <- ifelse(df_valcano$padj < ffdr, 
                            ifelse(abs(df_valcano$log2FoldChange) > flogfc, 
                                   ifelse(df_valcano$log2FoldChange > flogfc, 'Up', 'Down'), 
                                   'Not'),
                            'Not')
} else {
  df_valcano$sign <- ifelse(df_valcano$pvalue < fpval, 
                            ifelse(abs(df_valcano$log2FoldChange) > flogfc, 
                                   ifelse(df_valcano$log2FoldChange > flogfc, 'Up', 'Down'), 
                                   'Not'),
                            'Not')
}

#########################################################
# 实践
#########################################################
# 火山图
# 导入火山图需要的差异分析后的基因全部表格
head(diffa.edegr$resdf)
resdf.mrna <- diffa.edegr$resdf %>% filter(type == 'protein_coding')
dds.df.fc <- resdf.mrna %>% arrange(desc(log2FC)) %>% 
  distinct(Gene, .keep_all = T)
head(dds.df.fc)
##
df_valcano <- dds.df.fc %>% 
  dplyr::select(Gene,log2FC, PValue, FDR) %>% 
  dplyr::rename(Row.names=Gene, log2FoldChange=log2FC, padj=FDR)
df_valcano <- na.omit(df_valcano)
head(df_valcano)

# 需要标记的marker gene
label_gene <- c("FTH1","SLC40A1","STEAP3","FTL","ACSL4","GPX4") 
label_gene %in% df_valcano$Row.names %>% all
df_valcano %>% filter(Row.names %in% label_gene)

# 颜色
pal_npg()(9) %>% show_col()
jco <- pal_npg()(9)

# 画图
plot_Volcano_1(result = df_valcano, logFC = 1, 
               adj_P = 0.05, label_geneset = NULL)
# 
plot_Volcano_1(result = df_valcano, logFC = log2(1.5), 
               adj_P = 0.1, label_geneset = label_gene) %>% 
  ggplotGrob() %>% cowplot::plot_grid() 
ggsave("./last1.valcano.6mark.gene.pdf", width = 7, height = 7)


[引用于](https://zhuanlan.zhihu.com/p/588755211)
