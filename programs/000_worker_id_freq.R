# Code for sampling RAIS - Part I - worker id exploration
# Vitor Costa 
# September 2018

# Explores the distribution of digits along worker id - PIS. 


# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)

# Importing worker id column from raw panel
setwd(input.dir)
cols2read = c("pis","yr") #selects rsolumns to be read
panel_workerid = fread(input.file,colClasses = "character", select = cols2read) 
nrows = dim(panel_workerid)[1] #fetches the number of rows

###################
# ID exploration  #
###################

# splitting digits in pis
panel_workerid[,first := strsplit(pis,"")[[1]][1],by = pis]
panel_workerid[,second := strsplit(pis,"")[[1]][2],by = pis]
panel_workerid[,third := strsplit(pis,"")[[1]][3],by = pis]
panel_workerid[,fourth := strsplit(pis,"")[[1]][4],by = pis]
panel_workerid[,fifth := strsplit(pis,"")[[1]][5],by = pis]
panel_workerid[,sixth := strsplit(pis,"")[[1]][6],by = pis]
panel_workerid[,seventh := strsplit(pis,"")[[1]][7],by = pis]
panel_workerid[,eighth := strsplit(pis,"")[[1]][8],by = pis]
panel_workerid[,ninth := strsplit(pis,"")[[1]][9],by = pis]

# Reshaping table for tabulation
measure.vars = names(panel_workerid)[3:11]
panel_workerid = melt(panel_workerid, id.vars = c("pis","yr"), measure.vars = measure.vars, 
                      variable = "position", value  = "digit")

# Tabulation
tab = 100*xtabs(~ digit + position + year, panel_workerid)/nrows
tab

# Exporting table with frequencies
write.csv(tab, paste(metadata.dir,"000_digit_position_worker_id.csv",sep="/"))
