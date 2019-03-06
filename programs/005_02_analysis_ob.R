# Code for sampling RAIS - analysis II - Oaxaca Blinder
# Vitor Costa 
# November 2018

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(Counterfactual)
library(lfe)
library(stargazer)
library(ggplot2)
library(oaxaca)
library(doParallel)
library(foreach)

# Reading sample
setwd(output.dir)
input.file = paste("04_toanalysis",suffix,".csv",sep="")
data = fread(input.file,colClasses = "character",na.strings = "")

# Adjusting class of columns
setkey(data,yr,pis)
data = data[,lapply(.SD,as.numeric),.SDcols=names(data)]
data = data[mean_earn!=0&hired_wage!=0]

#########
# 2.1 Oaxaca-Blinder decomposition per year
#########

# Simple OB on whole sample
ob00 = oaxaca(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(yr)+factor(region)|group1,data=data,R=100,na.action=na.omit)
prov = list(ob00$twofold$overall,ob00$y)
save(prov,file=paste("001_ob_whole_sample",suffix,".RData",sep="")) 
rm(prov)
rm(ob00)

# Simple OB by year
base.reg = function(x){
  oaxaca(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(region)|group1,
         data=x,R=100,na.action=na.omit) 
}

ob01 = data.table(year=numeric(0),total=numeric(0),comp=numeric(0),se.comp=numeric(0),coef=numeric(0),se.coef=numeric(0))
for (y in c(2003:2014)){
  reg = base.reg(data[yr==y])
  total = reg$y$y.diff
  comp = reg$twofold$overall[1,2]
  se.comp = reg$twofold$overall[1,3]
  coef = reg$twofold$overall[1,4]
  se.coef = reg$twofold$overall[1,5]
  ob01 = rbind(ob01,list(y,total,comp,se.comp,coef,se.coef))
}

setwd(analysis.dir)
fwrite(ob01,file=paste("001_ob_yearly",suffix,".csv",sep="")) 
rm(ob01)

#########
# 2.2 Inputing individual FEs
#########
setwd(analysis.dir)
input.file = paste("000_fe",suffix,".csv",sep="")
fe.pis = fread(input.file)
fe.pis = unique(fe.pis[,c("pis","effect")])
data = merge(data,fe.pis,by="pis")


#########
# 2.3 Net of FE Oaxaca-Blinder decomposition per year
#########
# Simple OB on whole sample
ob00 = oaxaca(log(hwage1)-effect~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(yr)+factor(region)|group1,data=data,R=100,na.action=na.omit)
prov = list(ob00$twofold$overall,ob00$y)
save(prov,file=paste("001_net_ob_whole_sample",suffix,".RData",sep="")) 
rm(prov)
rm(ob00)

# Simple OB by year
base.reg = function(x){
  oaxaca(log(hwage1)-effect~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(region)|group1,
         data=x,R=100,na.action=na.omit) 
}

ob01 = data.table(year=numeric(0),total=numeric(0),comp=numeric(0),se.comp=numeric(0),coef=numeric(0),se.coef=numeric(0))
for (y in c(2003:2014)){
  reg = base.reg(data[yr==y])
  total = reg$y$y.diff
  comp = reg$twofold$overall[1,2]
  se.comp = reg$twofold$overall[1,3]
  coef = reg$twofold$overall[1,4]
  se.coef = reg$twofold$overall[1,5]
  ob01 = rbind(ob01,list(y,total,comp,se.comp,coef,se.coef))
}

setwd(analysis.dir)
fwrite(ob01,file=paste("001_net_ob_yearly",suffix,".csv",sep="")) 
rm(ob01)