# Directory parameters 
# Vitor Costa
# September 2018

home.dir = "/rdcprojects/br/jrc466-papers/public-private-wagegap"
output.dir = paste(home.dir,"/sample",sep="/")
input.dir = "/rdcprojects/br/programs/RAIS-conf/data/02_rais_raw_panel"
macros.dir = "/rdcprojects/br/programs/RAIS-conf/programs/03_rais_clean_panel/macros"
metadata.dir = paste(home.dir,"/metadata",sep="/")
analysis.dir = paste(home.dir,"/analysis_output",sep="/")
graphs.dir = paste(home.dir,"/graphs",sep="/")

# Debugging paraemter
debug = 1
suffix = ifelse(debug == 1, "_debug","")

# One percent sample
onepc = 1
suffix = ifelse(debug == 1 & onepc == 1, "_onepc",suffix)
noboot = ifelse(debug == 1 & onepc == 0, TRUE,TRUE)
