
library(raster)
library(rgdal)
library(rgeos)
library(geosphere)


setwd("E:/UMN/research/2020-7-23 6X Rosemount Ce Wheat 200 ft 80/2020-07-23_16-25-18/Ce_Group_WSR_200_80_Re/4_index/reflectance")
t = list.files(".",".tif$")
t

st = stack(raster(t[1]),raster(t[2]),raster(t[6]))
st

writeRaster(st,"ts.tif",overwrite=T)

# check resolution #
setwd("C:/Users/liang/Desktop/UMN/research/data/ft200/reflectance/")
t = list.files(".",".tif$")
length(t)
ra = raster(t[1])
ra




setwd("E:/UMN/research/2020-7-23 6X Rosemount Ce Wheat 200 ft 80/2020-07-23_16-25-18/preprocessing/images_0.01062m_Blue-Green-Red-RedEdge-NIR")
t = list.files(".",".tif$")
t

par(mfrow=c(2,2))
ra = raster(t[1])
ra


#### Step 4 -- reclassify labels ####
setwd("E:/UMN/research/data/Wheat Stem Rust Rosemount 2020/Visual disease scores")
bb45 = readOGR(".", "bd_box_45")

df = data.frame(matrix(0,ncol=6,nrow=length(bb45)))
for (i in 1:length(bb45)){
  
  df[i,1]=i
  if (bb45$score[i]=="0"){df[i,2]=0} else{df[i,2]=1}
}

colnames(df)=c("plotID", "binary_1", "score", 'resistance',"class3_2","class5")
head(df)

library(stringr)

n = str_extract(bb45$score[1], '[[:digit:]]+')
strsplit(bb45$score[1], n)[[1]][2]

for (i in 1:length(bb45)){
  
  n = str_extract(bb45$score[i], '[[:digit:]]+')
  r = strsplit(bb45$score[i], n)[[1]][2]
  
  df[i,3]=n
  df[i,4]=r
  
}
head(df)

table(df$binary_1)
dim(df)
table(df$score)
table(df$resistance)
df$resistance2 = df$resistance
head(df)
table(df$resistance2)

df$resistance2[df$resistance2=='MRMS']='MS'
df$resistance2[df$resistance2=='MRMSS']='MS'
df$resistance2[df$resistance2=='MRR']='MR'
df$resistance2[df$resistance2=='RMR']='MR'
df$resistance2[df$resistance2=='MSS']='S'
df$resistance2[df$resistance2=='RMRMS']='MS'
df$resistance2[df$resistance2=='SMS']='S'
df$resistance2[df$resistance2=='S / 10S']='S'
df$resistance2[df$resistance2=='R / 6']='R'
df$resistance2[df$resistance2=='RM5R']='MR'
df$resistance2[df$resistance2=='RMR / 45MSS']='MS'

df$binary_2 = df$resistance2
df$binary_2[df$binary_2=='MR'] ="R"
df$binary_2[df$binary_2=='MS'] ="S"
table(df$binary_2)

head(df)
df2 = df
df2 = df2[,-c(5,6)]
head(df2)
colnames(df2)[5]='resistance_class_4'

table(df2$score)
table(df2$resistance_class_4)
table(df2$score)
table(df2$binary_2)
dim(df2)

write.csv(df2,"labels.csv",row.names = F)



#### Step 3 -- clip reflectance maps by each bounding box####
setwd("E:/UMN/research/data/Wheat Stem Rust Rosemount 2020/Visual disease scores")
bb45 = readOGR(".", "bd_box_45")

ys = bb45$score
table(ys)


setwd("E:/UMN/research/2020-7-23 6X Rosemount Ce Wheat 200 ft 80/2020-07-23_16-25-18/Ce_Group_WSR_200_80_Re/4_index/reflectance")
setwd("E:/UMN/research/data")
t = list.files(".",".tif$")
t
all = stack(t[1])


b = stack(t[1])
g = stack(t[2])
rgb = stack(t[3])
nir = stack(t[4])
rdg = stack(t[5])
r = stack(t[6])


all = stack(r, g, b, rgb, nir, rdg)
all

### rgb ###
all = stack("E:/UMN/research/2020-7-23 6X Rosemount Ce Wheat 200 ft 80/M200_80/rgb/Ce_group_WSD_rgb_200ft_80_transparent_mosaic_group1.tif")

library(parallel)
library(doParallel)
library(foreach)

cl = makeCluster(detectCores()-2)
registerDoParallel(cl)
re = foreach(i=1:length(bb45),.packages = c('raster','rgdal'),.export=c('bb45'))%dopar%{
  
  ply = bb45[i,]
  cp = crop(all, ply)
  cp2 = mask(cp, ply)
  
  nm0 = 'WSR200'
  nm1 = bb45$block[i]
  nm2 = bb45$label[i]
  nm3 = bb45$score[i]
  nm = paste(nm0, "_", nm1, "_", nm2, "_", nm3,".tif",sep="")
  
  ls = list(cp2, nm)
  return(ls)
}
stopCluster(cl)

#dir.create("plots")
#setwd("./plots/")
#lapply(548:length(re), function(i) writeRaster(re[[i]][[1]], re[[i]][[2]], overwrite=F))

library(stringr)

setwd("./wsr")
dir.create('./image_200_bb45')
setwd("./image_200_bb45/")

#setwd('./wsr200/bb50')
for (i in 1:length(re)){
  nm0 = 'WSR200'
  nm1 = bb45$block[i]
  nm2 = bb45$label[i]
  nm3 = bb45$score[i]
  nm = paste(nm0, "_", nm1, "_", nm2, "_", nm3,"_ply",as.character(i),"_45",".tif",sep="")
  
  ts = try(writeRaster(re[[i]][[1]], nm, overwrite=T))
  
  if(class(ts)=="try-error"){
    nm2 = strsplit(nm," / ")
    nm3 = paste(nm2[[1]][1],"_",nm2[[1]][2])
    nm4 = str_replace_all(nm3, pattern=" ", repl="")
    writeRaster(re[[i]][[1]], nm4, overwrite=T)
  }
}
  


#### Step 2 -- get accurate bounding box ####
setwd("C:/Users/liang/Desktop//UMN/research/data/Wheat Stem Rust Rosemount 2020/Visual disease scores")
plts = readOGR('.', 'plots_All')
plts


## get plots centroids ##
plt1 = plts[1,]
ctd = gCentroid(plt1)
ctd

plts$ctd_long = 0
plts$ctd_lat = 0

ctds = ctd


for (i in 1:length(plts)){
  
  plt = plts[i,]
  ctd = gCentroid(plt)
  ctds = rbind(ctds, ctd)
  
  plts$ctd_lat[i] = ctd@coords[1,2]
  plts$ctd_long[i] = ctd@coords[1,1]
  
}

plot(plts)
plot(ctds, add=T, col="blue")

ctds = ctds[-1,]
ctds

ctds$block = plts$block
ctds$label = plts$plot_name
ctds$score = plts$score

## get coordinates of each bounding box of plot ##
df = data.frame(matrix(0, ncol=7))
colnames(df)=c('xmin', 'xmax', 'ymin','ymax', 'block', 'label','score')

pts = data.frame(matrix(0, ncol=2))
colnames(pts)=c('longitude','latitude')

for (i in 1:length(ctds)){
  
  ctd = ctds[i,]
  x = ctd@coords[1,1]
  y = ctd@coords[1,2]
  
  xmin = x - 0.55
  xmax = x + 0.55
  ymin = y - 0.55
  ymax = y + 0.55
  
  df[i,1] = xmin
  df[i,2] = xmax
  df[i,3] = ymin
  df[i,4] = ymax
  df[i,5] = ctds$block[i]
  df[i,6] = ctds$label[i]
  df[i,7] = ctds$score[i]
  
  
  id1 = (i-1)*4 + 1
  id2 = (i-1)*4 + 2
  id3 = (i-1)*4 + 3
  id4 = (i-1)*4 + 4
  
  pts[id1,1] = xmin
  pts[id1,2] = ymax
  
  pts[id2,1] = xmax
  pts[id2,2] = ymax
  
  pts[id3,1] = xmax
  pts[id3,2] = ymin
  
  pts[id4,1] = xmin
  pts[id4,2] = ymin
}

dim(df)
dim(pts)

head(df)
head(pts)

write.csv(df, 'plot_bb_55.csv',row.names = F)

## create bounding box from coordinates ##

bb=c()

for (i in 1:length(ctds)){
  
  id1 = (i-1)*4 + 1
  id4 = (i-1)*4 + 4
  
  pt = pts[id1:id4,]
  p = Polygon(pt)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  
  if (i==1){
    bb = sps
  } else
    {
    bb = rbind(bb, sps, makeUniqueIDs=T)
  }
}

bb
crs(bb) = crs(ctds)
plot(bb, add=T, col="red")
bbs = SpatialPolygonsDataFrame(bb, data=data.frame(IDs=1:length(bb)),match.ID = F)
bbs$block = ctds$block
bbs$label = ctds$label
bbs$score = ctds$score

writeOGR(bbs,".", "bd_box_55", driver="ESRI Shapefile",overwrite_layer = T)



#### Step 1 -- Match the plot label with the ground truth disease score ####
pl = readOGR(".","plots")
pl$ids = 1:length(pl)
sc = read.csv("scores.csv",header=T)
head(sc)
dim(sc)

pla = readOGR(".", "plots_All")
pla2 = pla[(pla$block==1)|(pla$block==4)|(pla$block==5),]  
pla2
table(pla2$block)

pl
pla2$score = 0
pla2$ids = 1000

pl
pla2


pa = rbind(pl,pla2)
pa
writeOGR(pa, ".", 'plots_All',driver="ESRI Shapefile", overwrite_layer = T)


pl$plot_name[pl$plot_name=='NULL']='ROB'
pl$plot_name[pl$plot_name=='DH075\r\n']='DH075'


plt2 = pl[pl$block==2,]
plt3 = pl[pl$block==3,]
plt6 = pl[pl$block==6,]

plt_sb = rbind(plt2, plt3, plt6)
plt_sb

df = cbind(data.frame(plt_sb$plot_name), data.frame(plt_sb$block), data.frame(plt_sb$ids))
head(df)
colnames(df)=c('label','rep','ids')
dim(df)

dim(sc)
head(sc)

length(unique(sc$label))
length(unique(df$label))

table(sc$label)
table(df$label)

head(sc)
head(df)

df$plt = paste(df$label, df$rep,sep="-")
sc$plt = paste(sc$label, sc$rep,sep="-")


df2 = df[order(df$plt),]
sc2 = sc[order(sc$plt),]
head(df2)
head(sc2)


df2$plt2 = sc2$plt
df2

ids = c()
for (i in 1:dim(df2)[1]){
  
  ts = identical(df2$plt[i],df2$plt2[i])
  ids = c(ids,ts)
}
table(ids)


df2$score = sc$score

df3 = df2[order(df2$ids),]
head(df3)

plt_sb2 = plt_sb[order(plt_sb$ids),]
plt_sb2
head(plt_sb2$ids)
head(df3$ids)

ts2 = c()
for (i in 1:length(df3$ids)){
  ts2 = c(ts2, identical(df3$ids[i], plt_sb2$ids[i]))
}
table(ts2)

plt_sb2$score = df3$score
writeOGR(plt_sb2, '.', "plots", driver="ESRI Shapefile", overwrite_layer = T)

#names(sc)=c("label", "rep", "score")
#write.csv(sc,"scores.csv",row.names = F)



#### Step 0 -- get plots for each block ####
ply = readOGR(".","plys")  
blk = readOGR(".", "blocks")


plot(blk, col="red")
plot(ply, add=T)

bk1 = blk[blk$Id==1,]
bk2 = blk[blk$Id==2,]
bk3 = blk[blk$Id==3,]
bk4 = blk[blk$Id==4,]
bk5 = blk[blk$Id==5,]
bk6 = blk[blk$Id==6,]

plot(bk1,add=T,col="blue")
plot(bk2,add=T,col="blue")
plot(bk3,add=T,col="blue")
plot(bk4,add=T,col="blue")
plot(bk5,add=T,col="blue")
plot(bk6,add=T,col="blue")

ply1 = crop(ply, bk1)
ply2 = crop(ply, bk2)
ply3 = crop(ply, bk3)
ply4 = crop(ply, bk4)
ply5 = crop(ply, bk5)
ply6 = crop(ply, bk6)

plot(ply1, add=T,col="orange")

ply1$block=1
ply2$block=2
ply3$block=3
ply4$block=4
ply5$block=5
ply6$block=6

ply_a = rbind(ply1,ply2, ply3, ply4, ply5, ply6)
ply_a

table(ply_a$block)

writeOGR(ply_a, ".",'plots_All',driver="ESRI Shapefile")
