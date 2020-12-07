


setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/host")

library(raster)  
library(rgdal)
library(NLMR)
library(landscapetools)
library(landscapemetrics)
library(foreach)
library(parallel)
library(doParallel)


ncol=1500
nrow=1500
resolution=100
proportion=0.5
rescale=T

############# Final Decesion #################


#### random cluster ####
clu=nlm_randomcluster(ncol=300,nrow=300,resolution=50,p=0.3,ai=c(0.9,0.1),neighbourhood = 4)
plot(clu)


prop1=c(0.99,0.95,0.90,0.70)
prop2=c(0.01,0.05,0.10,0.30)


t0=proc.time()
cl=makeCluster(4)
registerDoParallel(cl)

simu=foreach(k=1:4,.packages = c("NLMR","landscapetools","landscapemetrics","raster","igraph"),.export=c("prop1","prop2"))%dopar%{
  clu=nlm_randomcluster(ncol,nrow,resolution,p=0.3,ai=c(prop1[k],prop2[k]),neighbourhood = 4)
  return(clu)
}

proc.time()-t0
stopCluster(cl)

for (i in 1:4){
  ra=simu[[i]]
  nm=paste("RCp",as.character(i),"a.tif",sep="")
  writeRaster(ra,nm,overwrite=T)
}


#### fractal with 3 levels of autocorrelation ####
par(mfrow=c(1,3))

ncol=1500
nrow=1500
resolution=100

setwd("./smHost2/")

bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.1,rescale = T,user_seed = 88)
bm2=util_binarize(bm,breaks=c(0.3))
#plot(bm)
plot(bm2)

bm3 = bm
bm3[bm2==1]=0
plot(bm2)
plot(bm3)
sum(getValues(bm3/bm3),na.rm = T)/2250000

writeRaster(bm3, "F30AC1.tif",overwrite=T)


bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.3,rescale = T,user_seed = 88)
bm2=util_binarize(bm,breaks=c(0.3))
#plot(bm)
plot(bm2)

bm3 = bm
bm3[bm2==1]=0
plot(bm2)
plot(bm3)
sum(getValues(bm3/bm3),na.rm = T)/2250000

writeRaster(bm3, "F30AC3.tif",overwrite=T)


bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.5,rescale = T,user_seed = 88)
bm2=util_binarize(bm,breaks=c(0.03))
#plot(bm)
plot(bm2)

bm3 = bm
bm3[bm2==1]=0
#plot(bm2)
plot(bm3)
sum(getValues(bm3/bm3),na.rm = T)/2250000

writeRaster(bm3, "F03AC5.tif",overwrite=T)











rd2=util_binarize(rd,breaks=c(0.3))
plot(rd2)

fract_dim=c(0.01,0.25,0.5)
prop=c(0.01,0.05,0.1,0.3)


cl=makeCluster(4)
registerDoParallel(cl)
simu=foreach(k=1:4,.packages = c("NLMR","landscapetools","landscapemetrics","raster","igraph"),.export=c("prop","fract_dim"))%dopar%{
  bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.01,rescale = T,user_seed = k+100)
  bm2=util_binarize(bm,breaks=c(prop[k]))
  bm[bm2==1]=0
  return(bm)
}
proc.time()-t0
stopCluster(cl)

for (i in 1:4){
  ra=simu[[i]]
  nm=paste("fracProp",as.character(i),"AC1a.tif",sep="")
  writeRaster(ra,nm,overwrite=T)
}



cl=makeCluster(4)
registerDoParallel(cl)
simu=foreach(k=1:4,.packages = c("NLMR","landscapetools","landscapemetrics","raster","igraph"),.export=c("prop","fract_dim"))%dopar%{
  bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.01,rescale = T,user_seed = k+1000)
  bm2=util_binarize(bm,breaks=c(prop[k]))
  bm[bm2==1]=0
  return(bm)
}
proc.time()-t0
stopCluster(cl)

for (i in 1:4){
  ra=simu[[i]]
  nm=paste("fracProp",as.character(i),"AC1b.tif",sep="")
  writeRaster(ra,nm,overwrite=T)
}


cl=makeCluster(4)
registerDoParallel(cl)
simu=foreach(k=1:4,.packages = c("NLMR","landscapetools","landscapemetrics","raster","igraph"),.export=c("prop","fract_dim"))%dopar%{
  bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.01,rescale = T,user_seed = k+100000)
  bm2=util_binarize(bm,breaks=c(prop[k]))
  bm[bm2==1]=0
  return(bm)
}
proc.time()-t0
stopCluster(cl)

for (i in 1:4){
  ra=simu[[i]]
  nm=paste("fracProp",as.character(i),"AC1c.tif",sep="")
  writeRaster(ra,nm,overwrite=T)
}


cl=makeCluster(4)
registerDoParallel(cl)
simu=foreach(k=1:4,.packages = c("NLMR","landscapetools","landscapemetrics","raster","igraph"),.export=c("prop","fract_dim"))%dopar%{
  bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.01,rescale = T,user_seed = k+56000)
  bm2=util_binarize(bm,breaks=c(prop[k]))
  bm[bm2==1]=0
  return(bm)
}
proc.time()-t0
stopCluster(cl)

for (i in 1:4){
  ra=simu[[i]]
  nm=paste("fracProp",as.character(i),"AC1d.tif",sep="")
  writeRaster(ra,nm,overwrite=T)
}










############# Candidates #################
rect=nlm_randomrectangularcluster(ncol,nrow,minl=1,maxl =5,resolution)
cd=nlm_curds(curds = c(0.3, 0.7),recursion_steps = c(32, 6) )
plt=nlm_percolation(ncol,nrow,resolution,prob = 0.2)
nb=nlm_neigh(ncol,nrow,resolution,p_neigh = 0.4,p_empty = 0.2,categories = 2,proportions = c(0.3,0.7))

par(mfrow=c(2,2))


#plt, clu, random, mpd
plot(cd)
plot(clu)
plot(plt)
plot(nb)
plot(rect>0.8)

par(mfrow=c(2,2))
plot(cd>0)
plot(clu<1)
plot(plt>0)
plot(nb<1)
plot(rect>0.8)

gs=nlm_gaussianfield(ncol,nrow,resolution,autocorr_range = 7,nug=0.2)
plot(gs)

mdp=nlm_mpd(ncol,nrow,resolution,roughness = 0.55,rand_dev = 0.3)
bm=nlm_fbm(ncol,nrow,resolution, fract_dim =0.2,user_seed = 6,rescale = T)

plot(bm)
plot(bm>0.8)
plot(gs>0.8)
plot(mdp>0.8)
plot(rd>0.8)

gs=gs*10
plot(gs>5)

gs2=reclassify(gs,c(0,5.5,0),right=T)
plot(gs2)

gs3= (gs2-5.5)/(10-5.5)
plot(gs3)

gs3=gs3*(gs2/gs2)
gs3

plot(gs3)

extent(gs3)=c(98000,138000,98000,138000)

extent(gs3)=c(93500,128500,93500,128500)

plot(gs3)
gs3=gs3*10/2
gs3=as.integer(gs3)
plot(gs3)

ts=extend(gs3,y=extent(0,300000,0,300000),value=0)
plot(ts)
ts[is.na(ts)]=0
summary(ts)
plot(ts)
writeRaster(ts,"Infested.tif")
