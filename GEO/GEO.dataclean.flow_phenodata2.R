#  本脚本主要是对GEO数据的临床信息进行数据清洗
#  数据来源： 本脚本为四个gse的pd data 数据，为pd.data.1.R处理结果
#  文件输出： 本脚本输出四个数据集格子清洗后的数据，及一个合并后的总表
#  备注： 本脚本只修改数据整理数据内容，挑选想要的变量，观测样本基本不做挑选
rm(list = ls());gc()
library(dplyr)
library(data.table)
library(sva)

Sys.setenv('http_proxy'='http://127.0.0.1:7890')
Sys.setenv('https_proxy'='http://127.0.0.1:7890')

# 1. 读取数据
phen.pdata <- list.files(path = './data/',pattern = 'pdata.csv$', 
                         full.names = T) %>% 
  lapply(., read.csv, row.names = 1)

# 2. 分别清洗各个数据
#### 
phen1 <- phen.pdata[[1]]
phen1$Histology %>% table
#phen1 <- phen1 %>% filter(Histology == "Adenocarcinoma")
phen1[1:5,1:5]
phen1[,1]
phen1[,2] %>% table
phen1[,2] <- ifelse(phen.pdata[[1]][,2]=='M', 'Male', 'Female')
phen1[,3] %>% table
phen1[,4] %>% table
phen1[,4] <- sapply(phen1[,4], function(x) {
  switch(x,
         "IA" = "I",
         "IB" = "I",
         "IIA" = "II",
         "IIB" = "II",
         "IIIA" = "III",
         "IIIB" = "III")
  }, USE.NAMES = F)
phen.pdata[[1]][, 5:8]
phen.pdata[[1]][,9] %>% table
phen.pdata[[1]][,10:12] %>% table


# 此集合又正常样本
phen2 <- phen.pdata[[2]]
phen2$Tissue %>% table
#phen2 <- phen2 %>% filter(Tissue == "primary lung tumor")
phen2[,2] %>% table()
phen2[,2] <- phen2[, 2] %>% sapply(., function(x) {
    switch(x,
           "female" = "Female",
           "male" = "Male")
  }, USE.NAMES = F)
phen2[,3] %>% table 
phen2[,3] <- phen2[,3] %>% sapply(., function(x) {
  switch(x,
         "Ever-smoker" = "1",
         "Never-smoker" = "0")
}, USE.NAMES = F)
phen2[,4] %>% table
phen2[,4] <- phen2[,4] %>% sapply(., function(x) {
  switch(x,
         "IA" = "I",
         "IB" = "I",
         "II" = "II",
         "NA" = NA)
}, USE.NAMES = F)
phen2$Status %>% table
phen2$Status[phen2$Status == 'dead'] <- 1
phen2$Status[phen2$Status == 'alive'] <- 0
phen2$OS_day <- phen2$OS_day/365
phen2$Relapse %>% table()
phen2$Relapse <- ifelse(phen2$Relapse == 'relapsed', 1, 0)
phen2$Relapse_censor_day <- phen2$Relapse_censor_day/365

# # 含有鳞癌腺癌大细胞癌
phen3 <- phen.pdata[[3]]
phen3[1:5,1:5]
phen3$Histology %>% table
#phen3 <- phen3 %>% filter(Histology == "adeno")
phen3$Gender %>% table
phen3$Gender <- phen3$Gender %>% sapply(., function(x) {
  switch(x,
         "female" = "Female",
         "male" = "Male")
}, USE.NAMES = F)
phen3$Stage %>% table
phen3$Stage <- phen3$Stage %>% sapply(., function(x) {
  switch(x,
         "1a" = "I",
         "1b" = "I",
         "2a"= "II",
         "2b"= "II",
         "3a"= "III",
         "3b"= "III",
         "4"="IV")
}, USE.NAMES = F)
phen3$Status %>% table
phen3$Status <- ifelse(phen3$Status=='yes', 1, 0)
phen3$OS_day <- phen3$OS_day/365
phen3$Recurrence %>% table
phen3$Recurrence <- phen3$Recurrence %>% sapply(., function(x) {
  switch(x,
         "yes" = 1,
         "no" = 0,
         "not known" = NA)
}, USE.NAMES = F)
phen3$Recurrence_day <- as.numeric(phen3$Recurrence_day)/365
phen3$Adjuvant.treatment <- phen3$Adjuvant.treatment %>% 
  sapply(., function(x) {
    switch(x,
           "yes" = 1,
           "no" = 0,
           "not known" = NA)
  }, USE.NAMES = F)
phen3$Adjuvant.treatment
phen3$performance.status.corresponding.to.who.criteria


# # #
phen4 <- phen.pdata[[4]]
phen4$Histology %>% table
#phen4 <- phen4 %>% filter(Histology == "adenocarcinoma")
phen4$Age
phen4$Age <- round(phen4$Age)
phen4$Sex %>% table
phen4$Sex <- ifelse(phen4$Sex == 'F', 'Female', 'Male')
phen4$Smoking %>% table
phen4$T %>% table
phen4$T <- paste0('T', phen4$T)
phen4$N %>% table
phen4$N <- paste0('N', phen4$N)
phen4$M
phen4$M <- paste0('M', phen4$M)
phen4$Stage %>% table
phen4$Stage  <- phen4$Stage %>% sapply(., function(x) {
  switch(x,
         "1A" = "I",
         "1B" = "I",
         "2A"= "II",
         "2B"= "II")
}, USE.NAMES = F)
phen4$Status %>% table
phen4$Status <- ifelse(phen4$Status=="dead", 1, 0)
phen4$OS
phen4$Recurrence %>% table
phen4$Recurrence <- phen4$Recurrence %>% 
  sapply(., function(x) {
    switch(x,
           "Y" = 1,
           "N" = 0,
           "U" = NA)
  }, USE.NAMES = F)
phen4$Disease_free_survival_time
phen4$Histology

# 3. 整合数据及输出
# 3.1 为整合数据做准备，修改列名，及调整各表的列结构一致
names(phen1)[7:8] <- c('Status_p', 'PFS')
names(phen1)[1:9]
names(phen2)[1:9] <- names(phen1)[1:9]
names(phen3)
names(phen4)[c(1:3, 7:12)] <- names(phen1)[1:9]
# #
names(phen3)
phen3$Smoking <- NA
phen3 <- phen3 %>% dplyr::select(Age, Gender, Smoking, everything())
names(phen3)[1:9] <- names(phen1)[1:9]
# #
phenz1 <- phen1[, 1:9]
phenz2 <- phen2[, 1:9]
phenz3 <- phen3[, 1:9]
phenz4 <- phen4[c(1:3, 7:12)]

# 3.2 合并数据及输出
setid <- list.files(path = './data/',pattern = 'pdata.csv$') %>% 
  sub(pattern = '.pd.phen.pdata.csv', replacement='',.)
clean.clic.set <- list(phenz1, phenz2, phenz3, phenz4)
names(clean.clic.set) <- setid

for (i in 1:4) {
  clean.clic.set[[i]][, "GEOset"] <- names(clean.clic.set)[i]
  clean.clic.set[[i]][, "Sampleid"] <- rownames(clean.clic.set[[i]])
  outf <- paste0('data/clean_clic_', names(clean.clic.set)[i], '.csv')
  write.csv(clean.clic.set[[i]], file = outf, quote = F, row.names = T)
}

clean.clic.df <- do.call(rbind, clean.clic.set)

outf <- paste(setid, collapse = '_')
outf <- paste0('data/clean_clic_', outf, '.bind.csv')
write.csv(clean.clic.df, file = outf, quote = F, row.names = T)
