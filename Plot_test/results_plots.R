
setwd("C:/Users/liang/Desktop/opt/AllFiles3/summaries/")

library(reshape2)
library(ggplot2)
library(agricolae)

#### with different years####
dt = read.csv("InitInf.csv")
head(dt)

colnames(dt)[2]='Susptb'
dim(dt)

head(da3)
dt2 = dt[,-c(4,5)]


rd2 = melt(dt2)
head(rd2)


da = read.csv("dataAll.csv")
head(da)
dim(da)
colnames(da)[2]='Susptb'

dim(da)
da2 = da[,-c(4,8:14)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
head(da3)

da3 = da3[,-4]
da3$year = da$Year
da3$hp = da$hp
da3$perc = da$perc
da3$host = da$Host
head(da3)

rd2 = melt(da3,id.vars = c('year','hp','perc','host'))
head(rd2)

rd2$hp[rd2$hp=='FB']='FBM'
rd2$groups = unlist(lapply(1:dim(rd2)[1], function(x) paste(rd2$hp[x],rd2$variable[x],sep='_')))
head(rd2)
dim(rd2)
rd2b = rd2[complete.cases(rd2),]
dim(rd2)
colnames(rd2)[1]='Year'
ggplot(rd2, aes(groups, value,fill=Year))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern and Strategy", y = "Efficacy Compared to Random Treatment (%)")+theme(
    legend.text = element_text( size = 12)
  ) + stat_summary(fun=mean,aes(group=Year), position=position_dodge(.77),size=0.5,col="blue",fill='blue') 


head(rd2)

fb = rd2[rd2$hp=='FBM',]
rc = rd2[rd2$hp=='RC',]

anova(lm(value~Year+host+variable,fb))
anova(lm(value~Year+host+variable,rc))

#### with DP parameter ####
da = read.csv("DP_para_Summary.csv")
dim(da)
head(da)
colnames(da)[2]='Susptb'
da2 = da[,-c(5,9:11)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$Buffer = da$Buffer
head(da3)
da3b = da3[,-c(5)]
head(da3b)
da3b$host = da$hst
da3b$group=0

for (i in 1:dim(da3b)[1]){
  
  nm = substr(da3b$host[i],1,2)  
  if(nm=="FB"){nm0="FBM"}else(nm0=nm)
  da3b$group[i]=nm0
}

head(da3b)
da3b = da3b[,-c(7)]

rd2 = melt(da3b, id='group',variable.name = "Strategy")
head(rd2)
dim(rd2)

g1= ggplot(rd2, aes(group, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern", y = "Efficacy Compared to Random Treatment (%)")+theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Strategy),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 







#### with WC parameter ####
da = read.csv("WC_Summary.csv")
dim(da)
head(da)
colnames(da)[2]='Susptb'
summary(da)

da2 = da[,-c(5,9:11)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$Buffer = da$Buffer
head(da3)
da3b = da3[,-c(5)]
head(da3b)
da3b$host = da$hst
da3b$group=0

for (i in 1:dim(da3b)[1]){
  
  nm = substr(da3b$host[i],1,2)  
  if(nm=="FB"){nm0="FBM"}else(nm0=nm)
  da3b$group[i]=nm0
}

head(da3b)
da3b = da3b[,-c(7)]

rd2 = melt(da3b, id='group',variable.name = "Strategy")
head(rd2)
dim(rd2)

g2=ggplot(rd2, aes(group, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern", y = "Efficacy Compared to Random Treatment (%)")+theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Strategy),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 






library(ggpubr)
ggarrange(g1,g2,ncol=2, labels=c('a','b'),font.label = list(size = 18))

#### with different treatment buffer ####
da = read.csv("BufferSummary.csv")
colnames(da)[2]='Susptb'
head(da)
dim(da)
da$Buffer=0

for (i in 1:dim(da)[1]){
  hs = substr(da$id[i],1,2)  
  if(hs=="FB"){nm0="FBM"}else{nm0=hs}
  
  bf = substr(strsplit(da$id[i],"Bf")[[1]][2],1,1)
  da$Buffer[i]=paste(nm0,"_Buffer",as.numeric(bf)+1, sep="")
}

head(da)
table(da$Buffer)
da = da[!(da$Buffer=='FBM_Buffer2'),]
da = da[!(da$Buffer=='RC_Buffer2'),]
da$Buffer[da$Buffer=='FBM_Buffer3']='FBM_Buffer2'
da$Buffer[da$Buffer=='FBM_Buffer4']='FBM_Buffer3'
da$Buffer[da$Buffer=='RC_Buffer3']='RC_Buffer2'
da$Buffer[da$Buffer=='RC_Buffer4']='RC_Buffer3'


dim(da)
da2 = da[,-c(4,8:9)]

da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$Buffer = da$Buffer
head(da3)
da3b = da3[,-c(4)]
head(da3b)

dim(da3b)
da3b = da3b[complete.cases(da3b),]
dim(da3b)
da3b = da3b[-12,]
rd2 = melt(da3b, id.vars = "Buffer" , variable.name = "Strategy")
head(rd2)
dim(rd2)

ggplot(rd2, aes(Buffer, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern and Buffer Level", y = "Efficacy Compared to Random Treatment (%)")+
  theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Strategy),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 

head(rd2)

aggregate(value~Buffer+Strategy,rd2,FUN=mean)

#### with different budget ####

da = read.csv("BudgetSummary.csv")
head(da)
colnames(da)[2]='Susptb'
dim(da)

da2 = da[,-c(4,8:11)]

da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$bg = da$bg
da3$hsts = da$hst
da3$hsts[da3$hsts=="FB03AC3"]="FBM03AC3"
da3$hsts[da3$hsts=="FB10AC3"]="FBM10AC3"
head(da3)
da3b = da3[,-c(4)]


rd2 = melt(da3b, id.vars = c( "bg",'hsts') , variable.name = "Strategy")
head(rd2)
dim(rd2)

rd2$groups = 0

for (i in 1:dim(rd2)[1]){
  nm = substr(rd2$hsts[i], 1,2)
  
  if(nm=="FB"){nm0="FBM"}else(nm0='RC')
  
  
  rd2$groups[i]= paste(nm0,"_Budget",rd2$bg[i],sep="")
}  
head(rd2)


ggplot(rd2, aes(groups, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern and Budget Level", y = "Efficacy Compared to Random Treatment (%)")+
  theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Strategy),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 



head(rd2)
colnames(rd2)[5]="Level"

ggplot(rd2, aes(Strategy, value,fill=Level))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Selection Strategy", y = "Efficacy Compared to Random Treatment (%)")



da3$hp[da3$hp=='FB']='FBM'
head(da3)


c = list.files(".",".csv$")
c

da = read.csv("dataAll.csv")
head(da)
dim(da)


dim(da)
da2 = da[,-c(8:14)]

da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$Dispersal = da$Dispersal
da3[da3$Dispersal=="Disp0",]$Dispersal='Level1'
da3[da3$Dispersal=="Disp1",]$Dispersal='Level2'
da3[da3$Dispersal=="Disp2",]$Dispersal='Level3'
da3$hp = da$hp
da3$hp[da3$hp=='FB']='FBM'
head(da3)


#### with different LDD ####
dt = read.csv("dataAll.csv")
head(dt)
colnames(dt)[2]='Susptb'
dim(dt)
da2 = dt[,-c(4, 8:14)]

da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da=dt
da3$hp = da$hp
da3$Ldis = da$Ldis

head(da3)
#da3a = da3[,-c(4,5,9,10)]
da3a = da3[,-c(4)]
head(da3a)
rd2 = melt(da3a, id.vars = c( "hp",'Ldis') , variable.name = "Strategy")
head(rd2)
dim(rd2)

rd2$hp[rd2$hp=="FB"]="FBM"
rd2$group=0
for (i in 1:dim(rd2)[1]){
  nm =  paste(rd2$hp[i], "_",rd2$Ldis[i],sep="")
  rd2$group[i]=nm
}
head(rd2)
rd2$value=as.numeric(rd2$value)
ggplot(rd2, aes(group, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern and Level of LDD", y = "Efficacy Compared to Random Treatment (%)")+
  theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Strategy),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 





#### figure 4 ####

da = read.csv("dataAll.csv")
head(da)
colnames(da)[2]='Susptb'



dim(da)
da2 = da[,-c(4,8:14)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
head(da3)
da3$Dispersal=da$Dispersal
da3$hp = da$hp
da3$hp[da3$hp=='FB']="FBM"



da3b = da3[,-c(4)]
rd2 = melt(da3b, id.vars = c( "Dispersal",'hp') , variable.name = "Strategy")
head(rd2)
dim(rd2)
rd2$Dispersal[rd2$Dispersal=='Disp0']='Level1'
rd2$Dispersal[rd2$Dispersal=='Disp1']='Level2'
rd2$Dispersal[rd2$Dispersal=='Disp2']='Level3'
rd2$groups = 0

for (i in 1:dim(rd2)[1]){
  rd2$groups[i]= paste(rd2$hp[i],"_",rd2$Dispersal[i],sep="")
}  
head(rd2)

rd2
table(rd2$groups)


ggplot(rd2, aes(groups, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern and Dispersal Ability", y = "Efficacy Compared to Random Treatment (%)")+
  theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Strategy),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 

ggplot(rd2, aes(Strategy, value,fill=Dispersal))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern and Dispersal Ability", y = "Efficacy Compared to Random Treatment (%)")+
  theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Dispersal),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 





ts = melt(da3, id.vars = "nm")
head(ts)


ft = lm(value~variable, ts)
av1=aov(ft)
summary(av1)
tk1=HSD.test(av1,trt="variable",group=T,console=T)
tk1

head(da2)

#### Figure 3 ####
da = read.csv("dataAll.csv")
head(da)
dim(da)
colnames(da)[2]='Susptb'
da = da[da$Year!='Year3',]
dim(da)

fb = da[da$hp=="FB",]
rc = da[da$hp=="RC",]
dim(fb)
dim(rc)
t.test(fb$Hinf,fb$Susptb,paired=T)
t.test(rc$Hinf,rc$Susptb,paired=T)


dim(da)
da2 = da[,-c(4,8:14)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
head(da3)
#da3$hp = da$hp
da3$host = da$Host
#da3$ava = da$perc


aggregate(.~host, da3, FUN=median)


head(da3)
da3a = da3[,-4]
#da3a = da3[,-c(4,5,9,10)]
#da3a = da3[,-c(4,8,9)]
head(da3a)
rd2 = melt(da3a, id.vars = c( "host") , variable.name = "Strategy")
head(rd2)
dim(rd2)


rd2$host[rd2$host=="FB03AC1"]='FBM03AC1'
rd2$host[rd2$host=="FB03AC3"]='FBM03AC3'
rd2$host[rd2$host=="FB03AC5"]='FBM03AC5'
rd2$host[rd2$host=="FB10AC1"]='FBM10AC1'
rd2$host[rd2$host=="FB10AC3"]='FBM10AC3'
rd2$host[rd2$host=="FB10AC5"]='FBM10AC5'

rd2$host[rd2$host=="FBM03AC1"]='FBM_AC1_03%'
rd2$host[rd2$host=="FBM03AC3"]='FBM_AC3_03%'
rd2$host[rd2$host=="FBM03AC5"]='FBM_AC5_03%'
rd2$host[rd2$host=="FBM10AC1"]='FBM_AC1_10%'
rd2$host[rd2$host=="FBM10AC3"]='FBM_AC3_10%'
rd2$host[rd2$host=="FBM10AC5"]='FBM_AC5_10%'
rd2$host[rd2$host=="RC03"]='RC_03%'
rd2$host[rd2$host=="RC10"]='RC_10%'

head(rd2)


ts = da3
ts$host = da$hp
head(ts)
ts = ts[,-4]
ts = melt(ts, id.vars='host',variable.name = 'Strategy')
head(ts)


colnames(ts)=colnames(rd2)
rd2b = rbind(rd2,ts)
rd2b$host[rd2b$host=='FB']="FBM_All"
rd2b$host[rd2b$host=='RC']="RC_All"
head(rd2b)


ggplot(rd2b, aes(host, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern and Availability", y = "Efficacy Compared to Random Treatment (%)") +
  theme(
    legend.text = element_text( size = 12)
  )+ stat_summary(fun=mean,aes(group=Strategy),position=position_dodge(.77),size=0.5,col="blue",fill='blue') 

  ggplot(rd2, aes(host, value,fill=Strategy))  + geom_boxplot(size=1,outlier.shape=NA) +
  theme_classic() + theme(axis.text=element_text(size=12),
                          axis.title=element_text(size=14,face="bold")) +
  labs(x ="Host Pattern", y = "Efficacy Compared to Random Treatment (%)") +
  theme(
    legend.text = element_text( size = 12)
  )

da$hp=0
## preprocessing data ##
for (i in 1:dim(da)[1]){
  nm = da$Host[i]
  nm0 = substr(nm, 1,2)
  
  da$hp[i]=nm0
}


da$host2=0
da$perc=0
#da$host3 = 0
for (i in 1:dim(da)[1]){
  nm = da$Host[i]
  nm0 = substr(nm, 1,2)
  
  if (nm0=='FB'){
    hst = paste(nm0, substr(nm,5,7), sep="")
    pc = substr(nm, 3,4)
    h="FB"
  } else{
    hst = nm0
    pc = substr(nm,3,4)
    h='RC'
  }
  
  da$perc[i]=pc
  da$host2[i]=hst
}

head(da)


ts= rd2
