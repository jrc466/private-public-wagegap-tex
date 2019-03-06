# Code for sampling RAIS - filtering data 
# Vitor Costa 
# October 2018

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)

# Reading file
setwd(output.dir)
input.name=paste("03_tofilter",suffix,".csv",sep="")
sample = fread(input.name, colClasses = "character", na.strings = "")
setkey(sample,pis,yr) # ordering sample by pis and year

# Entries with conflicting ages have been removed
## 0 - Treating CEIs
sample[,c("cei_avail","cei"):=NULL]
sample = unique(sample)
## 1 - Flagging rural workers
sample[,rural:=ifelse(contract_type %in% c("20","25","70","75"),1,0)]
## 2 - Flagging not prime age (25-54)
sample[,prime_age:=ifelse(age1>= 25 & age1<=54,1,0)]
## 3 - Flagging sex and race conflict 
sample[,nsex:=uniqueN(sex),.(pis)]
sample[,nrace:=uniqueN(nonwhite),.(pis)]
### keeping mode sex
getmode = function(x){u = unique(x)
                      return(u[which.max(tabulate(match(x, u)))])}
sample[,sex1:=getmode(sex),.(pis)]
sample[,nonwhite1:=getmode(nonwhite),.(pis)]
## 4 - Excluding flagged observations
sample = sample[!is.na(nonwhite)&prime_age==1&rural==0&tempo==0&firmID_type=="1"]
## 5 - Pis's with more than one job
sample[,keep_job:=ifelse(max(mean_earn)==mean_earn,1,0),by=.(pis,yr)] # flagging main job as per highest monthly average
sample = sample[keep_job==1] # keeping only main job
# sample[,equal_pay:= length(unique(mean_earn)),.(pis,yr)] # all remaining duplicates have identical mean_earn
sample = sample[sample[,.I[1],.(pis,yr)]$V1] # keep first row of combinations pis X yr where keep_job=1


# Writing output
output.name = paste("04_toanalysis",suffix,".csv",sep="")
fwrite(sample,paste(output.dir,output.name,sep="/"),row.names=F, nThread=4)
