
setwd("C:/Users/liang/Desktop/opt/AllFiles3/summaries/")

library(reshape2)
library(ggplot2)
library(agricolae)

#### impact of host pattern, dispersal parameters####
da = read.csv("dataAll.csv")
head(da)
colnames(da)[2]='Susptb'
dim(da)
da2 = da[,-c(4,8:14)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
head(da3)

da3$hp = da$hp
da3$host = da$Host
da3$ac = da$host2
da3$perc = da$perc
da3$ldis = da$Ldis
da3$disp = da$Dispersal
head(da3)
da3b = da3[,-4]
head(da3b)
da3b$ac2 = da3b$ac
da3b$ac2[da3b$ac=="FBAC1"]=0.1
da3b$ac2[da3b$ac=="FBAC3"]=0.3
da3b$ac2[da3b$ac=="FBAC5"]=0.5
da3c = da3b[da3b$hp=='FB',]
da3c = da3b[da3b$hp=='RC',]
head(da3c)

rd2 = melt(da3b, id.vars=c('perc','host','hp','ldis','ac2','disp'))
head(rd2)

rd2$value=as.numeric(rd2$value)
rd2$ac2 = as.numeric(rd2$ac2)
rd2$perc = as.numeric(rd2$perc)
head(rd2)
ft = lm(value~variable+perc+ldis+disp,rd2)
summary(ft)
anova(ft)

#da3$ava = da$perc


fb = da[da$hp=="FB",]
rc = da[da$hp=="RC",]
dim(fb)
dim(rc)


#### impact of treatment buffer ####
da = read.csv("BufferSummary.csv")
head(da)
dim(da)
da$Buffer=0
da$hp = 0
da$host=0
for (i in 1:dim(da)[1]){
  hs = substr(da$id[i],1,2)  
  if(hs=="FB"){nm0="FBM"}else{nm0=hs}
  
  bf = substr(strsplit(da$id[i],"Bf")[[1]][2],1,1)
  da$Buffer[i]=paste("Buffer",as.numeric(bf)+1, sep="")
  da$host[i]=substr(da$id[i],1,4)
  da$hp[i]=nm0
}

head(da)
table(da$Buffer)
dim(da)
da2 = da[,-c(4,8:11)]

da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$Buffer = da$Buffer
da3$hp = da$hp
da3$host=da$host
head(da3)
da3b = da3[,-c(4)]
head(da3b)
da3b$Buffer[da3b$Buffer=='Buffer1']=0
da3b$Buffer[da3b$Buffer=='Buffer2']=50
da3b$Buffer[da3b$Buffer=='Buffer3']=150
da3b$Buffer[da3b$Buffer=='Buffer4']=300

dim(da3b)
da3b = da3b[complete.cases(da3b),]
dim(da3b)
head(da3b)
da3c= da3b[da3b$hp=="RC",]
head(da3c)
rd2 = melt(da3c, id.vars = c("Buffer",'hp','host') , variable.name = "Strategy")
head(rd2)
dim(rd2)

rd2$value=as.numeric(rd2$value)
rd2$Buffer=as.numeric(rd2$Buffer)
ft = lm(value~Strategy+Buffer+host, rd2)
anova(ft)



ds = unique(rd2$Buffer)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$Buffer==hst[i],]
  
  ft = lm(value~Strategy, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)
  
  fm1 <- aov(value~Strategy, dts)
  ts=TukeyHSD(fm1, "Strategy", ordered = TRUE)
  plot(TukeyHSD(fm1, "Strategy"))
  
  HSD.test(av1, trt = 'Strategy')
  
}

ft = lm(value~Strategy, rd2)
av1=aov(ft)
tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)


#### Impact of budget ####
da = read.csv("BudgetSummary.csv")
head(da)
dim(da)
da$hp = unlist(lapply(1:dim(da)[1],function(x) substr(da$hst[x],1,2)))

da2 = da[,-c(4,8:11)]

da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$bg = da$bg
da3$hsts = da$hst
da3$hp = da$hp
da3$hsts[da3$hsts=="FB03AC3"]="FBM03AC3"
da3$hsts[da3$hsts=="FB10AC3"]="FBM10AC3"
head(da3)
da3b = da3[,-c(4)]
table(da3b$bg)
da3b$bg[da3b$bg==1]=100
da3b$bg[da3b$bg==2]=200
da3b$bg[da3b$bg==3]=400

head(da3b)
table(da3b$hp)

da3c = da3b[da3b$hp=='RC',]
head(da3c)
rd2 = melt(da3c, id.vars = c( "bg",'hsts') , variable.name = "Strategy")
head(rd2)
dim(rd2)

rd2$value=as.numeric(rd2$value)
rd2$bg=as.numeric(rd2$bg)
ft = lm(value~Strategy+bg+hsts, rd2)
anova(ft)


#### with different budget ####

da = read.csv("BudgetSummary.csv")
head(da)
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
dim(rd2)



ds = unique(rd2$groups)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$groups==hst[i],]
  
  ft = lm(value~Strategy, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)
  
}


#### with DP parameter ####
da = read.csv("DP_para_Summary.csv")
dim(da)
head(da)

da2 = da[,-c(5,9:11)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd

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



for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = da3b[da3b$group==hst[i],]
  
  print(t.test(dts$Ip, dts$Ip_DP1, paired=T))
  
}


head(da3b)
rd2 = melt(da3b, id.vars = "group" , variable.name = "Strategy")
head(rd2)
dim(rd2)
ds = unique(rd2$group)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$group==hst[i],]
  
  ft = lm(value~Strategy, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)
  
  fm1 <- aov(value~Strategy, dts)
  ts=TukeyHSD(fm1, "Strategy", ordered = TRUE)
  plot(TukeyHSD(fm1, "Strategy"))
  
  HSD.test(av1, trt = 'Strategy')
  
}


#### with WC parameter ####
da = read.csv("WC_Summary.csv")
dim(da)
head(da)

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

ds = unique(da3b$group)
ds
hst = ds

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = da3b[da3b$group==hst[i],]
  
  print(t.test(dts$Ip, dts$Ip_WC1, paired=T))
  
}


head(da3b)
rd2 = melt(da3b, id.vars = "group" , variable.name = "Strategy")
head(rd2)
dim(rd2)
ds = unique(rd2$group)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$group==hst[i],]
  
  ft = lm(value~Strategy, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)
  
  fm1 <- aov(value~Strategy, dts)
  ts=TukeyHSD(fm1, "Strategy", ordered = TRUE)
  plot(TukeyHSD(fm1, "Strategy"))
  
  HSD.test(av1, trt = 'Strategy')
  
}

 #### with different treatment buffer ####
da = read.csv("BufferSummary.csv")
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
rd2 = melt(da3b, id.vars = "Buffer" , variable.name = "Strategy")
head(rd2)
dim(rd2)


ds = unique(rd2$Buffer)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$Buffer==hst[i],]
  
  ft = lm(value~Strategy, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)
  
  fm1 <- aov(value~Strategy, dts)
  ts=TukeyHSD(fm1, "Strategy", ordered = TRUE)
  plot(TukeyHSD(fm1, "Strategy"))
  
  HSD.test(av1, trt = 'Strategy')
  
}

ft = lm(value~Strategy, rd2)
av1=aov(ft)
tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)


#### with different budget ####

da = read.csv("BudgetSummary.csv")
head(da)
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
dim(rd2)



ds = unique(rd2$groups)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$groups==hst[i],]
  
  ft = lm(value~Strategy, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)
  
}


#### with different LDD ####
dt = read.csv("dataAll.csv")
head(dt)
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

rd2$groups = rd2$group
ds = unique(rd2$groups)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$groups==hst[i],]
  
  ft = lm(value~Strategy, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("Strategy"),group=T,console=T)
  
}

#### figure 4 ####
da = read.csv("dataAll.csv")

head(da)
da = da[da$Year!='Year3',]
dim(da)
da2 = da[,-c(4,8:14)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$Dispersal = da$Dispersal
da3$hp=da$hp
head(da3)

da3b = da3[,-c(4)]
da3b$year=da$Year
rd2 = melt(da3b, id.vars = c( "Dispersal",'hp','year') )
head(rd2)
dim(rd2)

rd2$groups = 0

for (i in 1:dim(rd2)[1]){
  rd2$groups[i]= paste(rd2$hp[i],"_",rd2$Dispersal[i],sep="")
}  
head(rd2)



ds = unique(rd2$groups)
ds
hst = ds
head(rd2)

for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = rd2[rd2$groups==hst[i],]
  
  ft = lm(value~variable, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("variable"),group=T,console=T)
  
}


library(multcompView)


ANOVA=aov(ft)

# Tukey test to study each pair of treatment :
TUKEY <- TukeyHSD(x=ANOVA, 'variable', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")


#### Figure 3 ####
da = read.csv("dataAll.csv")
head(da)
dim(da)
da2 = da[,-c(4,8:14)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
da3$hp = da$host2
da3$host = da$Host
da3$ava = da$perc
head(da3)
da3 = da3[,-c(4,7,9)]
ts = melt(da3, id.vars = "host")
head(ts)

head(rd2)
ts = rd2
ts$host = rd2$hp
hst = unique(ts$host)
hst
head(rd2)


da = read.csv("dataAll.csv")
head(da)
colnames(da)[2]='Susptb'
dim(da)
da2 = da[,-c(4,8:14)]
da3 = da2
da3 = (da3 - da3$Rd)*(-100)/da3$Rd
head(da3)

da3$hp = da$hp
da3$host = da$Host
da3$ac = da$host2
da3$perc = da$perc
da3$ldis = da$Ldis
da3$disp = da$Dispersal
head(da3)
da3b = da3[,-4]
head(da3b)
da3b$ac2 = da3b$ac
da3b$ac2[da3b$ac=="FBAC1"]=0.1
da3b$ac2[da3b$ac=="FBAC3"]=0.3
da3b$ac2[da3b$ac=="FBAC5"]=0.5
da3c = da3b[da3b$hp=='FB',]
da3c = da3b[da3b$hp=='RC',]
da3c=da3b
head(da3c)

da3c = da3c[da$Year!="Year3",]
head(da3c)
dim(da)
dim(da3c)

ts = da3c
ts$host = rd2$hp
hst = unique(ts$host)
hst



rd2 = melt(da3c, id.vars=c('perc','hp','host','ldis','disp'))
head(rd2)

rd2$value=as.numeric(rd2$value)
rd2$ac2 = as.numeric(rd2$ac2)
rd2$perc = as.numeric(rd2$perc)

ts=rd2
for (i in 1:length(hst)){
  
  print(i)
  print(hst[i])
  dts = ts[ts$host==hst[i],]
  
  ft = lm(value~variable+ldis+disp, dts)
  av1=aov(ft)
  tk1=HSD.test(av1,trt=c("variable",'ldis'),group=T,console=T)

}


