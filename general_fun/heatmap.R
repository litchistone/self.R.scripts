DEG_Pheat <- function(df, gene, pheno, outdir, f_mark='') {
  ## 得到热图数据所需格式
  #df = heat_matrix ; gene = sig_gene_100
  heat_matrix <- df[rownames(df) %in% gene,]
  # 设置顏色
  matdf <- pheno
  mat_colors <- list(type = c('brown','grey'))
  names(mat_colors$type) <- levels(matdf[,1])
  
  col <- colorRampPalette(brewer.pal(12,'Paired'))(24)
  #plot(1:24,rep(1,24),col= col,pch=16,cex=2)
  colp <- colorRampPalette(c(col[3],"white",col[11]))(100)
  
  fname <- paste0(outdir, '/heatmap.',f_mark,'.deg.pdf')
  pdf(fname, width = 12, height = 18)
  p <- pheatmap(heat_matrix, scale = 'row',cluster_rows = T,
                border_color = NA, show_colnames = T,
                show_rownames = T, fontsize = 11, fontsize_row = 7,
                annotation_col = matdf, annotation_colors = mat_colors,
                angle_col = "45",
                drop_levels = T, color = colp, name = 'Color',
                annotation_names_col = F)
  print(p)
  dev.off()
}
