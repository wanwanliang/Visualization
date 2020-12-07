
setwd("C:/Users/liang/Desktop/UMN/research//")

library(utils)

#
#zips = list.files(".",".zip$")
#zips
#for (i in 1:length(zips)){
  #unzip(zips[i])
#}



t = list.files(".",".tif$")
t

library(raster)
library(rgdal)

ra1 = stack(t[3])
ra1


setwd