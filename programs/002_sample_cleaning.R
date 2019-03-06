# Code for sampling RAIS - Part III - cleaning 
# Vitor Costa 
# October 2018

# Applies macros for cleaning the sample and creating consistency for worker and firm attributes across years.
# It does not validate worker ID and firm ID, yet

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)

# Reading file
setwd(output.dir)
input.file = paste("01_sample",suffix,".csv",sep="")
raw_panel = fread(input.file, colClasses = "character")
raw_panel[,V1:=NULL] # removing row names 
raw_panel = unique(raw_panel) # removing repeated rows

# Gathering macros 
setwd(macros.dir)
macros = list.files(pattern=".R")
macros = macros[3:length(macros)] #not checking firm and worker id just yet

# Applying macros
for (i in macros){
  cat("Executing macro ", i,"\n \n")
  source(i)
}

# Writing output
output.name = paste("02_clean",suffix,".csv",sep="")
fwrite(raw_panel,paste(output.dir,output.name,sep="/"), row.names = F)

