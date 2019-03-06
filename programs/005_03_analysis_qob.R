# Code for sampling RAIS - analysis III -Quantile Oaxaca Blinder
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
# 3. Oaxaca-Blinder counterfactual quantile
#########
#Changing coding for group1 and group2 
data[,group1:=ifelse(group1==1,0,1)]
data[,group2:=ifelse(group2==1,0,1)]

# Declaring basic formula
base.qob = function(x){
  counterfactual(log(hwage1)~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}

#First, using the whole sample
qob00 = base.qob(data)
save(qob00,file=paste("002_qob",suffix,".RData",sep=""))
rm(qob00)

#########
# 3.1 Oaxaca-Blinder counterfactual quantile - by gender and skill
#########
qob.gs = function(x){
  counterfactual(log(hwage1)~nonwhite1+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}

qob01 = qob.gs(data[sex1==1&low_skill==1])
qob02 = qob.gs(data[sex1==1&med_skill==1])
qob03 = qob.gs(data[sex1==1&high_skill==1])
qob04 = qob.gs(data[sex1==0&low_skill==1])
qob05 = qob.gs(data[sex1==0&med_skill==1])
qob06 = qob.gs(data[sex1==0&high_skill==1])

prov = list(qob01,qob02,qob03,qob04,qob05,qob06)
save(prov,file=paste("002_qob_genderskill",suffix,".RData",sep=""))
rm(prov)
rm(list=ls(pattern="qob"))

#########
# 3.2 Oaxaca-Blinder counterfactual quantile - by race and skill
#########
qob.rs = function(x){
  counterfactual(log(hwage1)~sex1+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}

qob07 = qob.rs(data[nonwhite1==1&low_skill==1])
qob08 = qob.rs(data[nonwhite1==1&med_skill==1])
qob09 = qob.rs(data[nonwhite1==1&high_skill==1])
qob10 = qob.rs(data[nonwhite1==0&low_skill==1])
qob11 = qob.rs(data[nonwhite1==0&med_skill==1])
qob12 = qob.rs(data[nonwhite1==0&high_skill==1])

prov = list(qob07,qob08,qob09,qob10,qob11,qob12)
save(prov,file=paste("003_qob_raceskill",suffix,".RData",sep=""))
rm(prov)
rm(list=ls(pattern="qob"))

#########
# 4 Inputing individual FEs
#########
setwd(analysis.dir)
input.file = paste("000_fe",suffix,".csv",sep="")
fe.pis = fread(input.file)
fe.pis = unique(fe.pis[,c("pis","effect")])
data = merge(data,fe.pis,by="pis")

#########
# 5 Recomputing QOB *~without~* fixed effects
#########

# 5.1 QOB on whole sample
base.qob = function(x){
  counterfactual(log(hwage1)-effect~nonwhite1+sex1+med_skill+high_skill+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}
qob00_net = base.qob(data)
save(qob00_net,file=paste("002_net_qob",suffix,".RData",sep=""))
rm(qob00_net)

# 5.2 QOB by gender and skill
qob.gs = function(x){
  counterfactual(log(hwage1)-effect~nonwhite1+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}
qob01_net = qob.gs(data[sex1==1&low_skill==1])
qob02_net = qob.gs(data[sex1==1&med_skill==1])
qob03_net = qob.gs(data[sex1==1&high_skill==1])
qob04_net = qob.gs(data[sex1==0&low_skill==1])
qob05_net = qob.gs(data[sex1==0&med_skill==1])
qob06_net = qob.gs(data[sex1==0&high_skill==1])

prov = list(qob01_net,qob02_net,qob03_net,qob04_net,qob05_net,qob06_net)
save(prov,file=paste("002_net_qob_genderskill",suffix,".RData",sep=""))
rm(prov)
rm(list=ls(pattern="qob"))

# 5.3 QOB by race and skill
qob.rs = function(x){
  counterfactual(log(hwage1)-effect~sex1+tenure+I(tenure^2)+factor(region)+factor(yr),
                 data=x,group=group1,sepcore=T,treatment=T,decomposition=T,ncore=detectCores()-1,noboot=noboot,nreg=100)
}
qob07_net = qob.rs(data[nonwhite1==1&low_skill==1])
qob08_net = qob.rs(data[nonwhite1==1&med_skill==1])
qob09_net = qob.rs(data[nonwhite1==1&high_skill==1])
qob10_net = qob.rs(data[nonwhite1==0&low_skill==1])
qob11_net = qob.rs(data[nonwhite1==0&med_skill==1])
qob12_net = qob.rs(data[nonwhite1==0&high_skill==1])

prov = list(qob07_net,qob08_net,qob09_net,qob10_net,qob11_net,qob12_net)
save(prov,file=paste("003_net_qob_raceskill",suffix,".RData",sep=""))
rm(prov)
rm(list=ls(pattern="qob"))