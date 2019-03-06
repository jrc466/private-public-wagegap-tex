# Code for sampling RAIS - creating variables for skill, temporary contracts and groups.
# Vitor Costa 
# October 2018

# Creates variables for skill, temporary contracts and groups. Also, auxiliary variables for filtering the data are added.

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)

# reading file
setwd(output.dir)
input.file = paste("02_clean",suffix,".csv",sep="")
sample = fread(input.file, colClasses = "character", na.strings = "")
setkey(sample,pis,yr) # ordering sample by pis and year

# Generating state and region
sample[,st:=substr(muni,1,2)] 
sample[,region:=substr(st,1,1)]

# Generating alternative establishment size
sample[establishment_size %in% as.character(c(0,1,2,3)),size:=1]
sample[establishment_size %in% as.character(c(4,5)),size:=2]
sample[establishment_size %in% as.character(c(6,7)),size:=3]
sample[establishment_size %in% as.character(c(8,9)),size:=4]

# Generating variable for skill
sample[,schooling := as.numeric(schooling)]
sample[,low_skill:= ifelse(schooling<7,1,0)] # some high school or less
sample[,med_skill:= ifelse(schooling>=7 & schooling<=8,1,0)] # high school and some college
sample[,high_skill:= ifelse(schooling>=9,1,0)] # college and up, including grad

# Inserting ppp
setwd(metadata.dir)
ppp = fread("002_ppp.csv",colClasses = "character")
sample = merge(sample,ppp,by="yr")

# Generating hourly wage
sample[,mean_earn:= as.numeric(mean_earn)]
sample[,hired_wage:= as.numeric(hired_wage)]
sample[,hired_hours:= as.numeric(hired_hours)]
sample[,ppp:= as.numeric(ppp)]
sample[,hwage1:= mean_earn/ppp/hired_hours]
sample[,hwage2:= hired_wage/ppp/hired_hours]

# Generating variable for Oaxa-Blinder groups
adm.public = as.character(c(1015,1023,1031,1040,1058,1066,1074,1082,1104,1112,1120,1139,1147,1155,1163,1171,1180,1198,1201,1210,1228,1236,1244,1252,1260,1279))
sample[,group1:= ifelse(legal_form %in% adm.public,0,1)] #adm public federal
sample[,group2:= ifelse(contract_type %in% c("30","31","35"),0,1)] #statutory, three levels
sample[,tempo:= ifelse(contract_type %in% c("50","60","65","90","95","96","97"),1,0)]

# Generating variable for age
## Step *1* - Generate year-of-birth
sample[,yob := substr(dob,nchar(dob)-3,nchar(dob)),by=pis] # generates year of birth when dob is reported
sample[,yob:=as.numeric(yob)]                              # age and yob to numeric
sample[,age:=as.numeric(age)]
sample[!(is.na(age)), yob:=as.numeric(yr)-as.numeric(age)] # generates year of birth when age is reported (reference year - age)
## Step *2* - Generate year-of-birth
conf = unique(sample[,.(pis,yob)], by=c("pis","yob"))      # keeping unique pairs of pis X yob
conf[,N:=.N,.(pis)]                                        # counting pis
rem.pis = as.vector(conf[N>2,pis])
## Step *3* - Flagging conflicting pis
sample[pis %in% rem.pis, pis_confage := 1][is.na(pis_confage), pis_confage:=0]
## Step *4* - Genrating Age
sample[,age1:=as.numeric(yr) - yob]

# Generating variable for race
sample[!is.na(race_color) & race_color%in%c("2","6"),white:=1]
sample[!is.na(race_color) & race_color%in%c("1","4","8"),white:=0]
sample[!is.na(race_color) & race_color%in%c("1","4"),black:=1]
sample[!is.na(race_color) & race_color%in%c("2","6","8"),black:=0]
sample[!is.na(race_color) & !race_color%in%c("2","9"),nonwhite:=1]
sample[!is.na(race_color) & race_color=="2",nonwhite:=0]

# Generating variable for public administration level
sample[,federal:= ifelse(legal_form %in% c("1015","1040","1074","1104","1139","1163","1198","1252"),1,0)]
sample[,state:= ifelse(legal_form %in% c("1023","1058","1082","1112","1147","1171","1236","1260"),1,0)]
sample[,municipal:=ifelse(legal_form %in% c("1031","1066","1120","1155","1180","1244","1279"),1,0)]

# Generating different power instances
sample[,exec:= ifelse(legal_form %in% c("1015","1023","1031"),1,0)]
sample[,legis:=ifelse(legal_form %in% c("1040","1058","1066"),1,0)]
sample[,judic:=ifelse(legal_form %in% c("1074","1082"),1,0)]

# clean schooling?
# clean race_color?
# clean tenure?

# Removing objects
rm(conf)

# writing output
output.file = paste("03_tofilter",suffix,".csv",sep="")
fwrite(sample[pis_confage==0],paste(output.dir,output.file,sep="/"),row.names=F,nThread = 4)

