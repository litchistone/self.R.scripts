#  本脚本主要是对GEO AFFY 芯片原始数据（ cel文件）读取，对其标准化，及 简单的质控展示
#  数据来源： 本脚本为四个gse的原始芯片数据，每一个gse为一个文件夹下含多个cel文件，为下载好的rawdata文件解压形成
#  文件输出： 本脚本输出一个合并后的标准化后的总表，及质控图片
#  备注： 本脚本只读取原始数据，标准化，直接合并表格，不做数据调整及挑选。
rm(list = ls());gc()
library(GEOquery);library(affyPLM)
library(affy);library(hgu133plus2cdf)
library(dplyr);library(RColorBrewer);library(tidyverse)

affy.qc <- function(raw, outdir) {
  # raw为affy::ReadAffy()结果，outdir为qc输出文件夹
  if (dir.exists(outdir)) {
    outdir <- paste0(outdir, '.', format(Sys.time(), "%y%m%d_%H%M%S"))
  }
  for (i in 1:length(raw)) {
    dir.create(paste0(outdir, "/qc_", names(raw)[i]), recursive = T)
    # 2. 查看数据质量
    # # 转换成画图需要的数据形式
    data.qc <- qc(raw[[i]]) #平均值法 # 需要simpleaffy包
    Pset <- fitPLM(raw[[i]])
    data.deg <- AffyRNAdeg(raw[[i]])
    
    pdf(file = paste0(outdir, "/qc_", names(raw)[i], '/',
                      names(raw)[i], '.qc.pdf'
    ), height = 16, width = 10)
    plot(data.qc)
    dev.off()
    # # 画图
    pdf(file = paste0(outdir, "/qc_", names(raw)[i], '/', 
                      names(raw)[i], '.rle&nuse&deg.pdf'), 
        height = 10, width = 16)
    par(mfcol = c(2, 1))
    RLE(Pset, main="RLE", las=3, col=colors)
    NUSE(Pset, col=colors, main="NUSE", las=3)
    # #
    par(mfcol = c(1, 1))
    plotAffyRNAdeg(data.deg, col=colors)
    legend("topleft", rownames(pData(raw[[i]])), col = colors, lwd=1,
           inset = 0.05, cex = 0.5)
    dev.off()
    
    cat(names(raw)[i], "qc Complete !\n")
  }
}

# 需要工作目录有这些文件夹存在， data/st res/
# 设置数据路径
p.raw.dir <- list.dirs('D:/Users/LZ/Downloads/GEO/GPL570.raw/meta/') # g
# #
raw.dir <- p.raw.dir %>% str_subset("GSE[:digit:]+_RAW$")            # h
gse <- raw.dir %>% str_extract("GSE[:digit:]+")

# 读取数据
data.raw <- lapply(raw.dir, function(x) {
  ReadAffy(celfile.path = x) #去读当前目录下所有的CEL文件
})
names(data.raw) <- gse
for (i in 1:length(data.raw)) {
  sampleNames(data.raw[[i]]) <- sampleNames(data.raw[[i]]) %>% 
    str_extract("GSM[:digit:]+")
}

# 标准化
data.rma <- lapply(data.raw, rma)
# 提取矩阵
eset.rma <- lapply(data.rma, exprs)
eset.rmadf <- do.call(cbind, eset.rma)
eset.rmadf[1:5,1:5]
dim(eset.rmadf)
out.name.rma <- paste0('data/st/clean_eset.', 
                       paste(names(eset.rma), collapse = '_'),'.rma.cbind.csv')
write.csv(eset.rmadf, file = out.name.rma, quote = F, row.names = T)


## 可视化 数据质量
colors <- brewer.pal(12, "Set3")
# 保存结果
pdf(file = 'res/pic.norm.rma.pdf', height = 10, width = 16)
par(mfcol = c(2, 1))
for (i in 1:length(eset.rma)) {
  out.name.rma <- paste0('data/st/', names(eset.rma)[i],'.rma.csv')
  write.csv(eset.rma[[i]], file = out.name.rma, quote = F, row.names = T)
  boxplot(eset.rma[[i]], col=colors, las=3, 
          main=paste0(names(eset.rma)[i], " After-RMA")) #经过RMA标准化后
}
dev.off()

cat("RMA Complete!")


######
# 质控 (耗时长)
affy.qc(raw = data.raw, outdir = 'qc')

###### 可选（慎重：耗时巨长）取消 下面代码注释即可 # # # 
# mas5标准化
data.mas <- lapply(data.raw, mas5) # 耗时很长
eset.mas <- lapply(data.mas, exprs)
eset.masdf <- do.call(cbind, eset.mas)
eset.masdf[1:5,1:5]
out.name.mas <- paste0('data/st/', 
                       paste(names(eset.rma), collapse = '_'),'.mas5.cbind.csv')
write.csv(eset.masdf, file = out.name.mas, quote = F, row.names = T)

# 保存结果
pdf(file = 'res/pic.norm.mas5.pdf', height = 10, width = 16)
par(mfcol = c(2, 1))
for (i in 1:length(eset.mas)) {
  out.name.mas <- paste0('data/st/', names(eset.mas)[i],'.mas5.csv')
  write.csv(eset.mas[[i]], file = out.name.mas, quote = F, row.names = T)
  boxplot(eset.mas[[i]], col=colors, las=3, 
          main=paste0(names(eset.mas)[i], " After-MAS5")) #经过RMA标准化后
}
dev.off()
####
