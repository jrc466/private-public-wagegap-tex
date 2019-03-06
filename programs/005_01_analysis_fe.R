# Code for sampling RAIS - analysis I - Mincer and Fixed Effects
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

# #########
# # 0. Getting a sense of the data via Mincerian equations
# #########
mincer_00 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|size+yr+region|0|region+size,data=data,exactDOF=T)
# mincer_01 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|establishment_size+yr+st|0|st+establishment_size,data=data,exactDOF=T)
mincer_02 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|yr+region|0|region,data=data,exactDOF=T)
# mincer_03 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)|yr+st|0|st,data=data,exactDOF=T)
# mincer_04 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|establishment_size+yr+region|0|region+establishment_size,data=data,exactDOF=T)
# mincer_05 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|establishment_size+yr+st|0|st+establishment_size,data=data,exactDOF=T)
# mincer_06 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|yr+region|0|region,data=data,exactDOF=T)
# mincer_07 = felm(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+age1+I(age1^2)|yr+st|0|st,data=data,exactDOF=T)
prov = list(mincer_00,mincer_02)
setwd(analysis.dir)
save(prov,file=paste("000_mincer",suffix,".RData",sep=""))
rm(prov)
rm(list=ls(pattern="mincer_"))

#########
# 1 Computing individual fixed effects
#########
pool = felm(log(hwage1)~med_skill+high_skill+tenure+I(tenure^2)|group1+yr+region+pis,data=data, na.action=na.omit)
fe.pis = as.data.table(getfe(pool))
fe.pis[,pis:=as.numeric(as.character(idx))]
fe.pis = fe.pis[fe=="pis",c("pis","effect")]
rm(pool)

#merging fe with data
data = merge(data,fe.pis,by="pis")

#saving fixed effects
setwd(analysis.dir)
fe.table = unique(data[,.(pis,effect,group1)])
fe.table[,group1 := as.character(group1)]
fwrite(fe.table,file=paste("000_fe",suffix,".csv",sep=""),nThread = 4)
##########################################################################################
