pz <- ggplot(df_valcano) +
  geom_point(aes(x=log2FoldChange, y=-1*log10(pvalue), 
                 color=sign,), size=1.2, alpha=0.5) +
  #geom_text(aes(x=log2FoldChange, y=-1*log10(pvalue), 
  #              color=sign, label=symbol),
  #          hjust=0, vjust=0, size=2) +
  geom_hline(yintercept=-log10(fpval), linetype=4)+
  geom_vline(xintercept=c(-flogfc, flogfc), linetype=4) +
  theme_bw() +
  scale_shape_manual(values = c(16,18)) +
  scale_y_continuous(
    name = "-log10(padj)", # Features of the first axis
    #sec.axis = sec_axis(~.*1, name="-log10(padj)"),# second axis features
    expand = c(0,0)) + 
  scale_x_continuous(name = 'log2(FoldChange)',limits = c(-13, 13)) +
  scale_color_manual(values = c(col[9],'grey',col[1]))+
  ggtitle("Valcano Group1 vs Group2") +
  theme(
    axis.title = element_text(color = 'black', size=13),
    #axis.title.y = element_text(color = 'black', size=13),
    #axis.title.y.right = element_text(color = 'black', size=13),
    axis.text = element_text(colour = 'black', size = 14),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())
pz %>% ggplotGrob() %>% cowplot::plot_grid()
#
ggsave(paste0(wk_dir,"/valcano_deg.pdf"), width = 6.5, height = 6)
