# 数据基本信息
htgsea[1:4,1:3]
##                                                    TCGA-05-4249-01A   TCGA-05-4382-01A   TCGA-05-4384-01A
##  Activated CD8 T cell                     0.5135670                  0.5938335                0.5021987       
##  Central memory CD8 T cell          0.6691930                  0.7729757                0.7180435      
##  Effector memeory CD8 T cell       0.4504921                  0.6284379                0.5687119       
# 转化格式（行作为观测样本，列为变量【即为：：样本不能是变量】） 
# ggplot2作图必须为长形数据，而长形数据的一个基础是观测必须为行，而观测值即为变量（如病人id为行【观测】，病人的临床或基因数据为列【变量】）
mydata <- t(as.data.frame(ssgsea.sca)) %>% as.data.frame()
mydata[1:4,1:3]  # 仍然是宽型数据（长型数据是：每行能确定一个唯一的观测值，即一行一个数据）
##                                          Activated CD8*      Central memory CD8*      Effector memeory CD8*
##  TCGA-05-4249-01A            0.5135670                 0.6691930                   0.4504921
##  TCGA-05-4250-01A            0.7935057                 0.7379094                   0.6415474
##  TCGA-05-4382-01A            0.5938335                 0.7729757                   0.6284379

# 添加分组信息
mydata$group <- annot_col[match(rownames(mydata),  rownames(annot_col)), 1]
# 宽变长
ggdata <- pivot_longer(data = mydata, cols = !group, names_to = "Cell",  values_to = "Value")
ggdata[1:3,1:3]
##  A tibble: 3 × 3
##  group    Cell                        Value
##  <fct>    <chr>                       <dbl>
##  cluster3 Activated CD8 T cell        0.514
##  cluster3 Central memory CD8 T cell   0.669
##  cluster3 Effector memeory CD8 T cell 0.450
# plot
ggplot(ggdata, aes(x=Cell, y=Value, fill=group))+
  geom_boxplot(width=0.7,size=0.3,outlier.color = NA)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  stat_compare_means(symnum.args = list(cutpoints = c(0,0.001, 0.01, 0.05, 1), 
                                        symbols = c("***", "**", "*", "ns")),
                     label = "p.signif")+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))+
  theme(legend.position = 'top')+
  xlab('')+ylab('Infiltration Abundance')+
  labs(fill='Group')
ggsave('./8.imu/barplot_ssGSEA.pdf', width = 16, height = 10)
