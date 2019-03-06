# Code for sampling RAIS - Graphs and Tables 
# Vitor Costa 
# November 2018

options(scipen=999)

# Loading config file
source("config.R", echo = T)

# Loading packages
library(data.table)
library(stargazer)
library(ggplot2)
library(ggpubr)

# Reading sample
setwd(output.dir)
input.file = paste("04_toanalysis",suffix,".csv",sep="")
data = fread(input.file,colClasses = "character",na.strings = "")

# Adjusting class of columns
setkey(data,yr,pis)
data = data[,lapply(.SD,as.numeric),.SDcols=names(data)]
data = data[mean_earn!=0&hired_wage!=0]

setwd(analysis.dir)

########
## 0. Descriptive Statistics 
########


########
## 1. FEs distribution 
########
obj_001 = fread(paste("000_fe",suffix,".csv",sep=""))
obj_001[,group1:=as.character(group1)]
obj_001[group1=="1",group1:="Public Administration"]
obj_001[group1=="0",group1:="Private Sector"]
graph_000 = ggplot(obj_001,aes(x=effect,fill=group1))+labs(fill="")+theme(legend.position=c(0.85,0.85),legend.title=element_blank())+ggtitle("Distribution of Worker FE's")+geom_density(alpha=0.8)+scale_fill_brewer(palette="Set1")+xlab("Fixed Effects (log)")
graph_000
ggsave(paste("001_fe",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

### For later, add table with descriptive statistics

########
## 2. Oaxaca Blinder 
########

# 2.1 Whole Sample OB 
load(paste("001_ob_whole_sample",suffix,".RData",sep=""),verbose=T)
prov[[1]][1,2]
prov[[1]][1,4]
prov[[2]]$y.diff

load(paste("001_net_ob_whole_sample",suffix,".RData",sep=""),verbose=T)
prov[[1]][1,2]
prov[[1]][1,4]
prov[[2]]$y.diff

# 2.2 Yearly OB
obj_002 = fread(paste("001_ob_yearly",suffix,".csv",sep=""))
setnames(obj_002,c("year","total","Characteristics","se.comp","Structural","se.coef"))
obj_002[,year:=as.character(year)]
obj_002 = melt(obj_002,id.vars = c("year"),measure.vars = c("Characteristics","Structural"))
graph_001 = ggplot(data=obj_002,aes(x=year,y=value,fill=variable))+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B")+xlab("") + ylab("Log of hourly wage")

obj_003 = fread(paste("001_net_ob_yearly",suffix,".csv",sep=""))
setnames(obj_003,c("year","total","Characteristics","se.comp","Structural","se.coef"))
obj_003[,year:=as.character(year)]
obj_003 = melt(obj_003,id.vars = c("year"),measure.vars = c("Characteristics","Structural"))
graph_002 = ggplot(data=obj_003,aes(x=year,y=value,fill=variable))+geom_bar(stat="identity")+geom_hline(yintercept=0,linetype="dashed")+scale_fill_brewer(palette="Paired")+theme_minimal()+theme(legend.title = element_blank())+ggtitle("Yearly O-B net of worker FE's")+xlab("") + ylab("Log of hourly wage")

ggarrange(graph_001,graph_002,ncol=1,nrow=2,common.legend=T,legend="bottom")
ggsave(paste("002_yearly_ob",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

########
## 3. Quantile Oaxaca Blinder 
########

suffix="_debug" ### jsut for now

# 3.1 QOB on whole sample
load(paste("002_qob",suffix,".RData",sep=""),verbose=T)
obj_004 = data.table(quantiles = qob00$quantiles, Total = qob00$total_effect, Structural = qob00$structral_effect, Characteristics = qob00$composition_effect)
obj_004 = melt(obj_004,id.vars = "quantiles")
graph_003 = ggplot(obj_004,aes(x=quantiles,y=value,group=variable))+ggtitle("Quantile O-B on Whole Sample")+geom_line(aes(linetype=variable),size=1.2)+xlab("Quantiles")+ylab("Log points")+scale_x_continuous(breaks=qob00$quantiles)+theme(legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

load(paste("002_net_qob",suffix,".RData",sep=""),verbose=T)
obj_005 = data.table(quantiles = qob00_net$quantiles, Total = qob00_net$total_effect, Structural = qob00_net$structral_effect, Characteristics = qob00_net$composition_effect)
obj_005 = melt(obj_005,id.vars = "quantiles")
graph_004 = ggplot(obj_005,aes(x=quantiles,y=value,group=variable))+ggtitle("Quantile O-B on Whole Sample - Net of Worker FE's")+geom_line(aes(linetype=variable),size=1.2)+xlab("Quantiles")+ylab("Log points")+scale_x_continuous(breaks=qob00$quantiles)+theme(legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

ggarrange(graph_003,graph_004,ncol=1,nrow=2,common.legend=T,legend="bottom")
ggsave(paste("003_qob",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# 3.2 QOB by Gender and Skill
load(paste("002_qob_genderskill",suffix,".RData",sep=""),verbose=T)

obj_006 = data.table(quantiles = prov[[1]]$quantiles, Total = prov[[1]]$total_effect, Structural = prov[[1]]$structral_effect, Characteristics = prov[[1]]$composition_effect)
obj_006 = melt(obj_006,id.vars = "quantiles")
graph_005 = ggplot(obj_006,aes(x=quantiles,y=value,group=variable))+ylab("Female")+ggtitle("Low Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[1]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_007 = data.table(quantiles = prov[[2]]$quantiles, Total = prov[[2]]$total_effect, Structural = prov[[2]]$structral_effect, Characteristics = prov[[2]]$composition_effect)
obj_007 = melt(obj_007,id.vars = "quantiles")
graph_006 = ggplot(obj_007,aes(x=quantiles,y=value,group=variable))+ggtitle("Medium Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[2]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_008 = data.table(quantiles = prov[[3]]$quantiles, Total = prov[[3]]$total_effect, Structural = prov[[3]]$structral_effect, Characteristics = prov[[3]]$composition_effect)
obj_008 = melt(obj_008,id.vars = "quantiles")
graph_007 = ggplot(obj_008,aes(x=quantiles,y=value,group=variable))+ggtitle("High Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[3]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_009 = data.table(quantiles = prov[[4]]$quantiles, Total = prov[[4]]$total_effect, Structural = prov[[4]]$structral_effect, Characteristics = prov[[4]]$composition_effect)
obj_009 = melt(obj_009,id.vars = "quantiles")
graph_008 = ggplot(obj_009,aes(x=quantiles,y=value,group=variable))+ylab("Male")+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[4]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_010 = data.table(quantiles = prov[[5]]$quantiles, Total = prov[[5]]$total_effect, Structural = prov[[5]]$structral_effect, Characteristics = prov[[5]]$composition_effect)
obj_010 = melt(obj_010,id.vars = "quantiles")
graph_009 = ggplot(obj_010,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[5]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_011 = data.table(quantiles = prov[[6]]$quantiles, Total = prov[[6]]$total_effect, Structural = prov[[6]]$structral_effect, Characteristics = prov[[6]]$composition_effect)
obj_011 = melt(obj_011,id.vars = "quantiles")
graph_010 = ggplot(obj_011,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[6]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

ggarrange(graph_005,graph_006,graph_007,graph_008,graph_009,graph_010,common.legend=T,ncol=3,nrow=2,legend="bottom")
ggsave(paste("003_qob_gs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# Net of FE's
load(paste("002_net_qob",suffix,".RData",sep=""),verbose=T)

obj_006 = data.table(quantiles = prov[[1]]$quantiles, Total = prov[[1]]$total_effect, Structural = prov[[1]]$structral_effect, Characteristics = prov[[1]]$composition_effect)
obj_006 = melt(obj_006,id.vars = "quantiles")
graph_005 = ggplot(obj_006,aes(x=quantiles,y=value,group=variable))+ylab("Female")+ggtitle("Low Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[1]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_007 = data.table(quantiles = prov[[2]]$quantiles, Total = prov[[2]]$total_effect, Structural = prov[[2]]$structral_effect, Characteristics = prov[[2]]$composition_effect)
obj_007 = melt(obj_007,id.vars = "quantiles")
graph_006 = ggplot(obj_007,aes(x=quantiles,y=value,group=variable))+ggtitle("Medium Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[2]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_008 = data.table(quantiles = prov[[3]]$quantiles, Total = prov[[3]]$total_effect, Structural = prov[[3]]$structral_effect, Characteristics = prov[[3]]$composition_effect)
obj_008 = melt(obj_008,id.vars = "quantiles")
graph_007 = ggplot(obj_008,aes(x=quantiles,y=value,group=variable))+ggtitle("High Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[3]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_009 = data.table(quantiles = prov[[4]]$quantiles, Total = prov[[4]]$total_effect, Structural = prov[[4]]$structral_effect, Characteristics = prov[[4]]$composition_effect)
obj_009 = melt(obj_009,id.vars = "quantiles")
graph_008 = ggplot(obj_009,aes(x=quantiles,y=value,group=variable))+ylab("Male")+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[4]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_010 = data.table(quantiles = prov[[5]]$quantiles, Total = prov[[5]]$total_effect, Structural = prov[[5]]$structral_effect, Characteristics = prov[[5]]$composition_effect)
obj_010 = melt(obj_010,id.vars = "quantiles")
graph_009 = ggplot(obj_010,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[5]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_011 = data.table(quantiles = prov[[6]]$quantiles, Total = prov[[6]]$total_effect, Structural = prov[[6]]$structral_effect, Characteristics = prov[[6]]$composition_effect)
obj_011 = melt(obj_011,id.vars = "quantiles")
graph_010 = ggplot(obj_011,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[6]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

ggarrange(graph_005,graph_006,graph_007,graph_008,graph_009,graph_010,common.legend=T,ncol=3,nrow=2,legend="bottom")
ggsave(paste("003_net_qob_gs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")


# 3.3 QOB by Race and Skill
load(paste("003_qob_raceskill",suffix,".RData",sep=""),verbose=T)

obj_006 = data.table(quantiles = prov[[1]]$quantiles, Total = prov[[1]]$total_effect, Structural = prov[[1]]$structral_effect, Characteristics = prov[[1]]$composition_effect)
obj_006 = melt(obj_006,id.vars = "quantiles")
graph_005 = ggplot(obj_006,aes(x=quantiles,y=value,group=variable))+ylab("Nonwhite")+ggtitle("Low Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[1]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_007 = data.table(quantiles = prov[[2]]$quantiles, Total = prov[[2]]$total_effect, Structural = prov[[2]]$structral_effect, Characteristics = prov[[2]]$composition_effect)
obj_007 = melt(obj_007,id.vars = "quantiles")
graph_006 = ggplot(obj_007,aes(x=quantiles,y=value,group=variable))+ggtitle("Medium Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[2]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_008 = data.table(quantiles = prov[[3]]$quantiles, Total = prov[[3]]$total_effect, Structural = prov[[3]]$structral_effect, Characteristics = prov[[3]]$composition_effect)
obj_008 = melt(obj_008,id.vars = "quantiles")
graph_007 = ggplot(obj_008,aes(x=quantiles,y=value,group=variable))+ggtitle("High Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[3]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_009 = data.table(quantiles = prov[[4]]$quantiles, Total = prov[[4]]$total_effect, Structural = prov[[4]]$structral_effect, Characteristics = prov[[4]]$composition_effect)
obj_009 = melt(obj_009,id.vars = "quantiles")
graph_008 = ggplot(obj_009,aes(x=quantiles,y=value,group=variable))+ylab("White")+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[4]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_010 = data.table(quantiles = prov[[5]]$quantiles, Total = prov[[5]]$total_effect, Structural = prov[[5]]$structral_effect, Characteristics = prov[[5]]$composition_effect)
obj_010 = melt(obj_010,id.vars = "quantiles")
graph_009 = ggplot(obj_010,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[5]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_011 = data.table(quantiles = prov[[6]]$quantiles, Total = prov[[6]]$total_effect, Structural = prov[[6]]$structral_effect, Characteristics = prov[[6]]$composition_effect)
obj_011 = melt(obj_011,id.vars = "quantiles")
graph_010 = ggplot(obj_011,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[6]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

ggarrange(graph_005,graph_006,graph_007,graph_008,graph_009,graph_010,common.legend=T,ncol=3,nrow=2,legend="bottom")
ggsave(paste("003_qob_rs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")

# Net of FE's
load(paste("003_net_qob_raceskill",suffix,".RData",sep=""),verbose=T)

obj_006 = data.table(quantiles = prov[[1]]$quantiles, Total = prov[[1]]$total_effect, Structural = prov[[1]]$structral_effect, Characteristics = prov[[1]]$composition_effect)
obj_006 = melt(obj_006,id.vars = "quantiles")
graph_005 = ggplot(obj_006,aes(x=quantiles,y=value,group=variable))+ylab("Nonwhite")+ggtitle("Low Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[1]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_007 = data.table(quantiles = prov[[2]]$quantiles, Total = prov[[2]]$total_effect, Structural = prov[[2]]$structral_effect, Characteristics = prov[[2]]$composition_effect)
obj_007 = melt(obj_007,id.vars = "quantiles")
graph_006 = ggplot(obj_007,aes(x=quantiles,y=value,group=variable))+ggtitle("Medium Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[2]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_008 = data.table(quantiles = prov[[3]]$quantiles, Total = prov[[3]]$total_effect, Structural = prov[[3]]$structral_effect, Characteristics = prov[[3]]$composition_effect)
obj_008 = melt(obj_008,id.vars = "quantiles")
graph_007 = ggplot(obj_008,aes(x=quantiles,y=value,group=variable))+ggtitle("High Skill")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[3]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_009 = data.table(quantiles = prov[[4]]$quantiles, Total = prov[[4]]$total_effect, Structural = prov[[4]]$structral_effect, Characteristics = prov[[4]]$composition_effect)
obj_009 = melt(obj_009,id.vars = "quantiles")
graph_008 = ggplot(obj_009,aes(x=quantiles,y=value,group=variable))+ylab("White")+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[4]]$quantiles)+theme(axis.title.x = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_010 = data.table(quantiles = prov[[5]]$quantiles, Total = prov[[5]]$total_effect, Structural = prov[[5]]$structral_effect, Characteristics = prov[[5]]$composition_effect)
obj_010 = melt(obj_010,id.vars = "quantiles")
graph_009 = ggplot(obj_010,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[5]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

obj_011 = data.table(quantiles = prov[[6]]$quantiles, Total = prov[[6]]$total_effect, Structural = prov[[6]]$structral_effect, Characteristics = prov[[6]]$composition_effect)
obj_011 = melt(obj_011,id.vars = "quantiles")
graph_010 = ggplot(obj_011,aes(x=quantiles,y=value,group=variable))+ggtitle("")+geom_line(aes(linetype=variable),size=1.2)+scale_x_continuous(breaks=prov[[6]]$quantiles)+theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.title=element_blank())+scale_linetype_manual(values=c("solid","dotted","twodash"))+geom_point(size=2) + geom_hline(aes(yintercept = 0))

ggarrange(graph_005,graph_006,graph_007,graph_008,graph_009,graph_010,common.legend=T,ncol=3,nrow=2,legend="bottom")
ggsave(paste("003_net_qob_rs",suffix,".pdf",sep=""),path=graphs.dir,dpi=600,width=210,units="mm")
