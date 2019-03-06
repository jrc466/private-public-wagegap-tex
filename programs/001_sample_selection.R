# Code for sampling RAIS - Part II - selecting sample
# Vitor Costa 
# September 2018

# Constructs a sample of the RAIS dataset based on the worker id. 
# Roughly, selecting two digits creates a sample of 1%; one digit, 10%. 

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(foreach)
library(doParallel)

setwd(input.dir)

# Setting up the cluster
no.cores = detectCores() #identifying the number of cores available
cat("There are ",no.cores," cores available.")
c1 = makeCluster(no.cores-1,outfile="") 
registerDoParallel(c1)

d = c(seq(0,781210787,100000),781210787)[-1]
e = fread("raw_panel.csv",nrows=0)
names = names(e)

sample10 = function(x){
  library(data.table)
  setwd(input.dir)
  prov = fread("raw_panel.csv",colClasses = "character", skip = x-100000,nrows = 100000)
  setnames(prov,names)
  prov[,spl:=substr(pis,nchar(pis)-3,nchar(pis)-3),pis]
  return(prov[spl=="7"])
}

sample0.1 = function(x){
  library(data.table)
  setwd(input.dir)
  prov = fread("raw_panel.csv",colClasses = "character", skip = x-100000,nrows = 100000)
  setnames(prov,names)
  prov[,spl:=substr(pis,nchar(pis)-6,nchar(pis)-4),pis]
  return(prov[spl=="438"])
}

if(debug==0){
  result = foreach(j=d, .combine=rbind) %dopar% sample10(j)   
} else {
  result = foreach(j=d, .combine=rbind) %dopar% sample0.1(j)
}

stopCluster(c1)

output.name = paste("01_sample",suffix,".csv",sep="")
fwrite(result, paste(output.dir,output.name,sep="/"),row.names = F)


# # Reading file
# setwd(input.dir)
# input.file = "raw_panel.csv"
# prov = fread(input.file, colClasses = "character")
# 
# # Functions to select sample. We'll keep obs with pis complying to
# # "5386" in positions 5-8, for a 0.01% sample; "5" in 5 for a 10%.
# 
# s_debug<-function(x) {
#   x[, spl_comb:=substr(pis,nchar(pis)-6,nchar(pis)-3),pis]
#   x[, spl:=ifelse(spl_comb=="5386",1,0)]
# }
# 
# s<-function(x) {
#   x[, spl_comb:=substr(pis,nchar(pis)-6,nchar(pis)-6)]
#   x[, spl:=ifelse(spl_comb=="5",1,0)]
# }
# 
# # Selecting sample
# if(debug==1){ s_debug(prov) } else { s(prov) }
# 
# # Ordering data
# setkey(prov,pis,year)
# 
# # Exporting sample
# output.name = paste("sample",suffix,sep="")
# fwrite(prov[spl==1], paste(output.dir,output.name,sep="/"),row.names = F)

