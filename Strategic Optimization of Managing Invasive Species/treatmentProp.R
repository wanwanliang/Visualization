

setwd("../.")
t= list.files(".",".tif$")
t
bf0 = list.files(".","Bf0")
bf1 = list.files(".","Bf1")
bf3 = list.files(".","Bf3")

dir.create("bf0")
dir.create("bf1")
dir.create("bf3")
file.copy(bf0,'./bf0')
file.copy(bf1,'./bf1')
file.copy(bf3,'./bf3')

setwd("C:/Users/liang/Desktop/opt/AllFiles3/result/buffer/bf3")

#### Step 1 -- remove uncessary files ####
hst2 = list.files(".","Hst2.tif")
hst0 = list.files(".","Hst0.tif")
ip = list.files(".","Ip.tif")

rm = c(hst0,hst2,ip)
rm
file.remove(rm)
ip2 = list.files(".","Ip2.tif")
ip2k = list.files(".","Ip2k.tif")
f03 = list.files(".","03")
rm = intersect(f03, ip2)
rm
file.remove(rm)
rm = list.files(".","Year3")
file.remove(rm)

#### Step 2 -- calculate ####

hinf_tif = list.files(".","Hinf.tif")
hst_tif = list.files(".","Hst.tif")
ip_tif = list.files(".","Ip")
rd_tif = list.files(".","Rd.tif")
wvfrt1_tif = list.files(".","Wvfrt1.tif")
wvfrt2_tif = list.files(".","Wvfrt2.tif")

df0 = data.frame(matrix(0,ncol=6,nrow=8))
df1 = data.frame(matrix(0,ncol=6,nrow=8))
df2 = data.frame(matrix(0,ncol=6,nrow=8))
colnames(df0)=c('Hinf',"Susptb",'Ip','Rd','Wvfrt1','Wvfrt2')
colnames(df1)=c('Hinf',"Susptb",'Ip','Rd','Wvfrt1','Wvfrt2')
colnames(df2)=c('Hinf',"Susptb",'Ip','Rd','Wvfrt1','Wvfrt2')

nms = unlist(lapply(1:length(hinf_tif), function(x) strsplit(hinf_tif[x],'Hinf')[[1]][1]))

df0$nms=nms
df1$nms=nms
df2$nms=nms



buffer=300
pixelArea=10000
budget=2000000
cost_per_meter_sq=1
cost_per_meter_sq = 1
treatment_years=c(2019)

start_time <- "2019-01-01"
end_time <- "2019-12-30"


repros = 4
disp = 10
longdis = 0.97 

anthropogenic_distance_scale[[1]] = 8000 
reproductive_rate[[1]] = 4
natural_distance_scale[[1]] = 10
percent_natural_dispersal[[1]] = 0.97
use_anthropogenic_kernel = TRUE
cost_per_meter_sq = 1

#### Loop ####
library(rgeos)
library(rgdal)

for (i in 1:8){
  
  hinf_ra = raster(hinf_tif[i])
  hst_ra = raster(hst_tif[i])
  ip_ra = raster(ip_tif[i])
  rd_ra = raster(rd_tif[i])
  wvfrt1_ra = raster(wvfrt1_tif[i])
  wvfrt2_ra = raster(wvfrt2_tif[i])
  
  df0[i,1]= sum(getValues(hinf_ra/hinf_ra),na.rm=T)
  df0[i,2]= sum(getValues(hst_ra/hst_ra),na.rm=T)
  df0[i,3]= sum(getValues(ip_ra/ip_ra),na.rm=T)
  df0[i,4]= sum(getValues(rd_ra/rd_ra),na.rm=T)
  df0[i,5]= sum(getValues(wvfrt1_ra/wvfrt1_ra),na.rm=T)
  df0[i,6]= sum(getValues(wvfrt2_ra/wvfrt2_ra),na.rm=T)
  
  
  rd= try(random_pixel(rd_ra,buffer,budget,cost_per_meter_sq))
  if (class(rd)=='try-error'){
    rd=list(c(0),c(0))
    rd[[1]][1]=sum(getValues(rd_ra/rd_ra),na.rm=T)
    rd[[2]][1]=sum(getValues(rd_ra),na.rm=T)
  }
  print("RDdone")
  
  hinf = try(Hinfest_pixel(hinf_ra,buffer,budget,cost_per_meter_sq))
  
   if (class(hinf)=='try-error'){
    hinf=list(c(0),c(0))
    hinf[[1]][1]=sum(getValues(hinf_ra/hinf_ra),na.rm=T)
    hinf[[2]][1]=sum(getValues(hinf_ra),na.rm=T)
    }
  print("Hinfdone")
  
  
  hst =try(threat_pixel(hst_ra,width=2000,host=host,budget,buffer,cost_per_meter_sq)) 
  if (class(hst)=='try-error'){
    hst=list(c(0),c(0))
    hst[[1]][1]=sum(getValues(hst_ra/hst_ra),na.rm=T)
    hst[[2]][1]=sum(getValues(hst_ra),na.rm=T)
  }
  print("Hstdone")
  
  
  wvfrt1=try(wvfrt_pixel(wvfrt1_ra,buffer,budget,cost_per_meter_sq))
  if (class(wvfrt1)=='try-error'){
    wvfrt1=list(c(0),c(0))
    wvfrt1[[1]][1]=sum(getValues(wvfrt1_ra/wvfrt1_ra),na.rm=T)
    wvfrt1[[2]][1]=sum(getValues(wvfrt1_ra),na.rm=T)
  }
  
  print("wvfrt1done")
  
  wvfrt2=try(wvfrtHzd(wvfrt2_ra, 8,host,buffer,budget,cost_per_meter_sq, a=natural_distance_scale[[1]], rep=reproductive_rate[[1]], np=8,20))
  if (class(wvfrt2)=='try-error'){
    wvfrt2=list(c(0),c(0))
    wvfrt2[[1]][1]=sum(getValues(wvfrt2_ra/wvfrt2_ra),na.rm=T)
    wvfrt2[[2]][1]=sum(getValues(wvfrt2_ra),na.rm=T)
  }
  print("wvfrt2done")
  
  ip_rank_ply = ip_rank(ip_ra,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,2000 ,8)
  ip2 = try(ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, ip_ra))
  if (class(ip2)=='try-error'){
    ip2=list(c(0),c(0))
    ip2[[1]][1]=sum(getValues(ip_ra/ip_ra),na.rm=T)
    ip2[[2]][1]=sum(getValues(ip_ra),na.rm=T)
  }
  print("IPdone")
  
  
  
  df1[i,1]=hinf[[1]][1]
  df2[i,1]=hinf[[2]][1]
  
  df1[i,2]=hst[[1]][1]
  df2[i,2]=hst[[2]][1]
  
  df1[i,3]=ip2[[1]][1]
  df2[i,3]=ip2[[2]][1]
  
  df1[i,4]=rd[[1]][1]
  df2[i,4]=rd[[2]][1]
  
  df1[i,5]=wvfrt1[[1]][1]
  df2[i,5]=wvfrt1[[2]][1]
  
  df1[i,6]=wvfrt2[[1]][1]
  df2[i,6]=wvfrt2[[2]][1]
  df1
  df2
  print(i)
}

df0
df1
df2


#### first year ####
  
inf = raster(infected_file)
hinf_ra=inf
hst_ra=inf
ip_ra=inf
rd_ra=inf
wvfrt1_ra=inf
wvfrt2_ra=inf
i=9
df0[i,1]= sum(getValues(hinf_ra/hinf_ra),na.rm=T)
df0[i,2]= sum(getValues(hst_ra/hst_ra),na.rm=T)
df0[i,3]= sum(getValues(ip_ra/ip_ra),na.rm=T)
df0[i,4]= sum(getValues(rd_ra/rd_ra),na.rm=T)
df0[i,5]= sum(getValues(wvfrt1_ra/wvfrt1_ra),na.rm=T)
df0[i,6]= sum(getValues(wvfrt2_ra/wvfrt2_ra),na.rm=T)

buffer=300
rd= random_pixel(rd_ra,buffer,budget,cost_per_meter_sq)
print("RDdone")
hinf = Hinfest_pixel(hinf_ra,buffer,budget,cost_per_meter_sq)
print("Hinfdone")
hst =threat_pixel(hst_ra,width=2000,host=host,budget,buffer,cost_per_meter_sq) #30000
print("Hstdone")


wvfrt1=wvfrt_pixel(wvfrt1_ra,buffer,budget,cost_per_meter_sq)
print("wvfrt1done")
wvfrt2=wvfrtHzd(wvfrt2_ra, 8,host,buffer,budget,cost_per_meter_sq, a=natural_distance_scale[[1]], rep=reproductive_rate[[1]], np=8,20)
print("wvfrt2done")
ip_rank_ply = ip_rank(ip_ra,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,2000 ,8)
ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)
print("IPdone")

df1[i,1]=hinf[[1]][1]
df2[i,1]=hinf[[2]][1]

df1[i,2]=hst[[1]][1]
df2[i,2]=hst[[2]][1]

df1[i,3]=ip2[[1]][1]
df2[i,3]=ip2[[2]][1]

df1[i,4]=rd[[1]][1]
df2[i,4]=rd[[2]][1]

df1[i,5]=wvfrt1[[1]][1]
df2[i,5]=wvfrt1[[2]][1]

df1[i,6]=wvfrt2[[1]][1]
df2[i,6]=wvfrt2[[2]][1]

df0
df1
df2

write.csv(df0,"Initial_inf.csv",row.names = F)
write.csv(df1,"Treat_InfestedPixelN.csv",row.names = F)
write.csv(df2,"Total_inf_treat.csv",row.names = F)
