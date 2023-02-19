#  本脚本主要是下载GEO数据的临床信息，并进行简单的过滤
#  数据来源： GEOquery在线下载
#  文件输出： 本脚本输出两个表，一个为原始下载的表格，一个为取子集的目标表格
#  备注： 本脚本只做了简单的取子集操作，无数据格式调整，详细数据清洗见 GEO.dataclean.flow_phenodata2.R
rm(list = ls())
library(GEOquery)
library(affyPLM)
library(affy)
library(RColorBrewer)
library(dplyr)
library(tidyverse)
library(hgu133plus2cdf)

Sys.setenv('http_proxy'='http://127.0.0.1:7890')
Sys.setenv('https_proxy'='http://127.0.0.1:7890')

# 1. 读取数据
#gse <- "GSE30219"
gse <- "GSE68465"
# #
gset <- getGEO(gse, getGPL = F, destdir = "./geo")
pd <- pData(gset[[1]])
pd %>% View()
pd$title
#str_extract(pd$title, pattern = "\\D*") %>% table()

# 2. 整理分组信息,并导出指定的数据矩阵
pd[1:4,1:4];dim(pd)
write.csv(pd, file = paste0(gse, '.pd.phen.odata.csv'), quote = F, row.names = T)

# 
pdz <- pd[, 48:ncol(pd)]
names(pdz)

names(pdz) <- sub(pattern = ":ch1", replacement = "", names(pdz))
names(pdz)
pdz$disease_stage %>% table

write.csv(pdz, file = paste0(gse, '.pd.phen.pdata.csv'), 
          quote = F, row.names = T)
