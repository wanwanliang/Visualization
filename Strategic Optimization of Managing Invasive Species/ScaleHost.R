

setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/host/smHost/")
library(raster)
library(rgdal)

t = list.files(".",".tif$")
t

ra = raster(t[1])
ra

########## Format old random cluster ###########
rc3 = raster("RC03.tif")
rc10 = raster("RC10.tif")
rc30 = raster("RC30.tif")

plot(rc3)
plot(rc10)
plot(rc30)

extent(rc3)

extent(rc3)=extent(0, 320000, 0, 320000)
extent(rc10)=extent(0, 320000, 0, 320000)
extent(rc30)=extent(0, 320000, 0, 320000)

rc3b =crop(rc3, extent(ra))
rc10b =crop(rc10, extent(ra))
rc30b =crop(rc30, extent(ra))

plot(rc3b)
plot(rc10b)
plot(rc30b)

writeRaster(rc3b, "RCp03.tif")
writeRaster(rc10b, "RCp10.tif")
writeRaster(rc30b, "RCp30.tif")

########### set scale and total number of host ###############


f1 = raster("F30AC1.tif")
f3 = raster("F30AC3.tif")
f5 = raster("F30AC5.tif")
r3 = raster("RCp30.tif")

par(mfrow=c(2,2))
plot(f1)
plot(f3)
plot(f5)
plot(r3)

sum(getValues(f1/f1),na.rm = T)
sum(getValues(f3/f3),na.rm = T)
sum(getValues(f5/f5),na.rm = T)
sum(getValues(r3/r3),na.rm = T)

df = sum(getValues(f1/f1),na.rm = T) - sum(getValues(r3/r3),na.rm = T)

# df > 0
r3n=r3
r3n[r3>0]=NA
plot(r3n)
r3n[r3n==0]=1

sp = sampleRandom(r3n, size=abs(df), na.rm=T, sp=T, asRaster=T)
plot(sp)
sp =sp *0
sp[is.na(sp)]=1
plot(sp)

r3b = r3+ sp*50

# df < 0
r3n=r3
r3n[r3==0]=NA
plot(r3n)

sp = sampleRandom(r3n, size=abs(df), na.rm=T, sp=T, asRaster=T)
plot(sp)
sp =sp *0
sp[is.na(sp)]=1
plot(sp)

r3b = r3*sp
sum(getValues(r3b/r3b),na.rm = T)

writeRaster(r3b, "RC30.tif")

######## set total amount of host #####

f1 = raster("F30AC1.tif")
f3 = raster("F30AC3.tif")
f5 = raster("F30AC5.tif")
r3 = raster("RC30.tif")

sum(getValues(f1),na.rm=T)
sum(getValues(f3),na.rm=T)
sum(getValues(f5),na.rm=T)
sum(getValues(r3),na.rm=T)

plot(f1)
plot(f3)
plot(f5)

f1[f1==0]=NA
f3[f3==0]=NA
f5[f5==0]=NA

f1b = stretch(f1, 0, 100)
f3b = stretch(f3, 0, 100)
f5b = stretch(f5, 0, 100)

par(mfrow=c(1,3))
plot(f1b)
plot(f3b)
plot(f5b)


sum(getValues(f1b),na.rm=T)
sum(getValues(f3b),na.rm=T)
sum(getValues(f5b),na.rm=T)
sum(getValues(r3),na.rm=T)

f1b[f1b<50]=f1b[f1b<50]*2
f3b[f3b<50]=f3b[f3b<50]*2
f5b[f5b<50]=f5b[f5b<50]*2

f1c = f1b
f3c = f3b*sum(getValues(f1b),na.rm=T)/sum(getValues(f3b),na.rm=T)
f5c = f5b*sum(getValues(f1b),na.rm=T)/sum(getValues(f5b),na.rm=T)
r3c = r3*sum(getValues(f1b),na.rm=T)/sum(getValues(r3),na.rm=T)

sum(getValues(f1c),na.rm=T)
sum(getValues(f3c),na.rm=T)
sum(getValues(f5c),na.rm=T)
sum(getValues(r3c),na.rm=T)


f1c = as.integer(f1c)
f3c = as.integer(f3c)
f5c = as.integer(f5c)
r3c = as.integer(r3c)
f1c = as.integer(f1c*sum(getValues(f3c),na.rm=T)/sum(getValues(f1c),na.rm=T))

f1c[is.na(f1c)]=0
f3c[is.na(f3c)]=0
f5c[is.na(f5c)]=0
r3c[is.na(r3c)]=0

writeRaster(f1c, "FB30AC1.tif")
writeRaster(f3c, "FB30AC3.tif")
writeRaster(f5c, "FB30AC5.tif")
writeRaster(r3c, "R30.tif")


wc = stack("smWCfinal.tif")
extent(wc) = extent(c(0,320000,0,320000))

wc2 = crop(wc, extent(f1c))
wc2
writeRaster(wc2, "WeatherCoefficient.tif")

inf= raster("smInf_Initialsm.tif")
extent(inf) = 
writeRaster(inf, "Inf.tif")

inf2 = crop(inf, extent(c(340000, 365000, 60000, 80000)))
plot(inf2)

sum(getValues(inf2/inf2),na.rm = T)

plot(f1c)

inf2

extent(inf2) = c(50000, 75000, 125000, 145000)

inf3= extend(inf2, f1c)
par(mfrow=c(1,2))

plot(inf3)
plot(f1c)

ply = rasterToPolygons(inf3, function(x){x>0})
plot(ply,add=T,border="red")

inf3[is.na(inf3)]=0

writeRaster(inf3, "Inf.tif")
