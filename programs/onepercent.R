# Code for sampling RAIS - Part II - selecting sample
# Vitor Costa 
# September 2018

# Constructs a 1% sample of the RAIS dataset based on the worker id.
# Input file should be the 10% sample, for now

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(foreach)
library(doParallel)

setwd(output.dir)

# Setting up the cluster
no.cores = detectCores() #identifying the number of cores available
cat("There are ",no.cores," cores available.")
c1 = makeCluster(no.cores-1,outfile="") 
registerDoParallel(c1)

d = c(seq(0,78111332,100000),78111332)[-1]
e = fread("01_sample.csv",nrows=0)
names = names(e)

sample1pc = function(x){
  library(data.table)
  setwd(output.dir)
  prov = fread("01_sample.csv",colClasses = "character", skip = x-100000,nrows = 100000)
  setnames(prov,names)
  prov[,spl:=substr(pis,nchar(pis)-5,nchar(pis)-5),pis]
  return(prov[spl=="3"])
}

result = foreach(j=d, .combine=rbind) %dopar% sample1pc(j)
stopCluster(c1)

output.name = paste("01_sample",suffix,".csv",sep="")
fwrite(result, paste(output.dir,output.name,sep="/"),row.names = F)
