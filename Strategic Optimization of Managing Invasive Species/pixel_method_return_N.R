

## Define Function Input Variable ##
# inf: the infested file in raster format, 
# buffer: the buffer size around each treatment pixel
# width: the buffer size within which the total number of uninfested host would be derived
# ply: the polygon converted from the infested raster, the main use of ply is to exclude the infested host
# pixelArea: the area of each pixel
# distance_classes: only used for Method 5 to indicate how many classes your want to cluster the infested pixel based on 
#  their distance to the wavefront.
# Wcoef: mean weather coefficient during the simulation period
# Nstep: possible number of reproduction times 
###################### Raster to polygons ##############################

ra2plyMerge <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',pypath="C:\\Program Files\\GDAL\\gdal_polygonize.py") {
  
  require(raster)
  require(rgdal)
  
  x=reclassify(x,c(1,Inf,1))
  
  
  dir=getwd()
  on.exit(setwd(dir))
  setwd(dirname(pypath))
  
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  
  writeRaster(x, {f <- tempfile(fileext='.tif')})
  rastpath <- normalizePath(f)
  
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  shp <- readOGR(dirname(outshape), layer = basename(outshape))
  crs(shp)=crs(x)
  
  shp=shp[shp$DN!=0,]
  
  polys=shp
  
  require(rgeos)
  require(maptools)
  
  disMatr=gDistance(polys,polys,byid=T)
  disMatr=as.data.frame(disMatr)
  n=dim(disMatr)[1]
  
  polys$id=1:n
  
  for (i in 1:n){
    polys$id[disMatr[,i]==0]=polys$id[i]
  }
  
  groups=polys$id
  polys2=unionSpatialPolygons(polys,groups)
  n2=length(polys2)
  
  polys3=SpatialPolygonsDataFrame(polys2,data=data.frame(ID=1:n2), match.ID = F)
  return(polys3)
  
  setwd(dir)
}



########### Method 1 -- Randomly Select Infested Pixel for Treatment ################
## Randomly select infested pixels based on budget and cost per unit ##

random_pixel=function(inf,buffer,budget,cost_per_meter_sq){
  
  area=budget/cost_per_meter_sq
  
  pixelArea=xres(inf)*yres(inf)
  
  inf2=inf
  inf2[inf2==0]=NA
  
  n=floor(area/pixelArea)+2
  
  rd=sampleRandom(inf2,size=n,na.rm=T,asRaster=T)
  rd_ply=rasterToPolygons(rd,dissolve=F)
  ply_bf=buffer(rd_ply,width=buffer,dissolve=F)
  if (length(ply_bf)< n){n = length(ply_bf)}
  
  ply_bf$Cumu_Area=0
  for (i in 1:n){
    trt=gUnionCascaded(ply_bf[1:i,])
    ply_bf$Cumu_Area[i]=gArea(trt)
  }
  
  treatment=ply_bf[ply_bf$Cumu_Area<= area & ply_bf$Cumu_Area!=0,]
  treatment=gUnionCascaded(treatment)
  
  df=area-gArea(treatment)
  nontr=ply_bf[ply_bf$Cumu_Area> area,]
  
  if (df>0){
    crds=gCentroid(nontr[1,])
    crds_bf=buffer(crds,width=sqrt(df/pi))
    treatment=gUnion(treatment,crds_bf)
  }
  
  ginter = gIntersection(treatment, rd_ply)
  if (class(ginter)[1]=="SpatialCollections"){
    ginter = ginter@polyobj
    ginter=gUnionCascaded(ginter)
  }
  
  trtN= gArea(ginter)/pixelArea
  inft = extract(inf, ginter, fun=sum, na.rm=T)
  
  ls = list(trtN, inft)
  
  return(ls)
}
# excuation example
# treatRd_Pixel= random_pixel(inf,budget,cost_per_meter_sq,pixelArea)



########### Method 2 -- Select infested pixels for treatment based on total number #######
## of uninfested host within the given width of buffer area of each pixel ##

# idealy, the width should be choosen based on the dispersal ability of targeted species
Thost <- function(inf, ra_ply,width,host){
  
  host[inf>0]=0
  #host = host/host
  host[host>0]=1
  ra_ply$id = 1:length(ra_ply)
  ply_bf = buffer(ra_ply,width,dissolve=F)
  #ply2=gUnionCascaded(ply)
  #ply2=gBuffer(ply2,width=0)
  
  buff_only = gDifference(ply_bf, ra_ply,  byid=c(T,F), id=as.character(ply_bf$id))
  hostN = unlist(lapply(1:length(buff_only), function(x){extract(host, buff_only[x,], na.rm=T, fun=sum)}))
  hst=as.data.frame(unlist(hostN))
  
  hst=as.data.frame(unlist(hostN))
  hst$id = as.numeric(names(buff_only))
  colnames(hst)=c("Thst","id")
  ts = merge(ra_ply, hst,by="id")
  ts$Thst[is.na(ts$Thst)]=0
  ra_ply = ts
  
  return(ra_ply)
}

threat_pixel=function(inf,width,host, budget, buffer, cost_per_meter_sq){
  
  area=budget/cost_per_meter_sq
  pixelArea=xres(inf)*yres(inf)
  
  ra_ply=rasterToPolygons(inf,fun=function(x){x>0},dissolve = F)
  
  ra_ply=Thost(inf, ra_ply,width,host)
  ra_ply2=ra_ply[order(ra_ply$Thst,decreasing = T),]
  
  ply_bf=buffer(ra_ply2,width=buffer,dissolve=F)
  n=floor(area/pixelArea)
  ply_bf$Cumu_Area=0
  
  n=floor(area/pixelArea)+2
  if (length(ply_bf)< n){n = length(ply_bf)}
  
  for (i in 1:n){
    trt=gUnionCascaded(ply_bf[1:i,])
    ply_bf$Cumu_Area[i]=gArea(trt)
  }
  
  treatment=ply_bf[ply_bf$Cumu_Area<= area & ply_bf$Cumu_Area!=0,]
  treatment=gUnionCascaded(treatment)
  
  df=area-gArea(treatment)
  nontr=ply_bf[ply_bf$Cumu_Area> area,]
  
  if (df>0){
    crds=gCentroid(nontr[1,])
    crds_bf=buffer(crds,width=sqrt(df/pi))
    treatment=gUnion(treatment,crds_bf)
  }
  
  
  ginter = gIntersection(treatment, ra_ply)
  if (class(ginter)[1]=="SpatialCollections"){
    ginter = ginter@polyobj
    ginter=gUnionCascaded(ginter)
  }
  
  trtN= gArea(ginter)/pixelArea
  inft = extract(inf, ginter, fun=sum, na.rm=T)
  
  ls = list(trtN, inft)
  
  return(ls)
}
## excuation example ##
## treatTrt_Pixel= threat_pixel(inf,width=1000,ply,host,budget,cost_per_meter_sq,pixelArea)


########## Method 3 -- Select highest infested pixel  ###########
Hinfest_pixel = function(inf,buffer,budget,cost_per_meter_sq){
  
  area=budget/cost_per_meter_sq
  pixelArea=xres(inf)*yres(inf)
  
  
  ra_ply=rasterToPolygons(inf,fun=function(x){x>0},dissolve = F)
  names(ra_ply) = "infest_level"
  
  
  ra_ply2=ra_ply[order(ra_ply$infest_level,decreasing = T),]
  ply_bf=buffer(ra_ply2,width=buffer,dissolve=F)
  
  
  n=floor(area/pixelArea)+2
  if (length(ply_bf)< n){n = length(ply_bf)}
  
  ply_bf$Cumu_Area=0
  for (i in 1:n){
    trt=gUnionCascaded(ply_bf[1:i,])
    ply_bf$Cumu_Area[i]=gArea(trt)
  }
  
  treatment=ply_bf[ply_bf$Cumu_Area<= area & ply_bf$Cumu_Area!=0,]
  treatment=gUnionCascaded(treatment)
  
  df=area-gArea(treatment)
  nontr=ply_bf[ply_bf$Cumu_Area> area,]
  
  if (df>0){
    crds=gCentroid(nontr[1,])
    crds_bf=buffer(crds,width=sqrt(df/pi))
    treatment=gUnion(treatment,crds_bf)
  }
  
  ginter = gIntersection(treatment, ra_ply)
  if (class(ginter)[1]=="SpatialCollections"){
    ginter = ginter@polyobj
    ginter=gUnionCascaded(ginter)
  }
  
  trtN= gArea(ginter)/pixelArea
 
  inft = extract(inf, ginter, fun=sum, na.rm=T)
  
  ls = list(trtN, inft)
  
  return(ls)
  
}
## excuation example ##
## treatTrt_Pixel= Hinfest_pixel(inf,buffer,budget,cost_per_meter_sq,pixelArea)


########## Method 4 -- Select infested pixel at wave front ###########

# get wavefront
wvfrt = function(inf){
  ## Derive wave front 
  
  library(concaveman)
  
  pts=rasterToPoints(inf,fun=function(x){x>0},spatial = T)
  
  crds=pts@coords
  concave=concaveman(crds,concavity = 1)
  p=Polygon(concave)
  ps=Polygons(list(p),1)
  sps=SpatialPolygons(list(ps))
  crs(sps)=crs(pts)
  sps=SpatialPolygonsDataFrame(sps,data=data.frame(ID=1))
  
  # lns is used as the wave front
  lns=as(sps,"SpatialLinesDataFrame")
  return(lns)
}

wvfrt_pixel = function(inf,buffer,budget, cost_per_meter_sq){
  
  pixelArea=xres(inf)*yres(inf)
  
  # get wavefront, lns is spatial Line
  lns=wvfrt(inf)
  
  ra_ply=rasterToPolygons(inf,fun=function(x){x>0},dissolve = F)
  distance=gDistance(lns, ra_ply, byid=T)
  
  ra_ply$dis_frtwv=distance[1:length(ra_ply)]
  ra_ply2=ra_ply[order(ra_ply$dis_frtwv,decreasing = F),]
  
  ply_bf= buffer(ra_ply2,width=buffer,dissolve=F)
  
  area=budget/cost_per_meter_sq
  n=floor(area/pixelArea)
  ply_bf$Cumu_Area=0
  
  n=floor(area/pixelArea)+2
  if (length(ply_bf)< n){n = length(ply_bf)}
  
  
  for (i in 1:n){
    trt=gUnionCascaded(ply_bf[1:i,])
    ply_bf$Cumu_Area[i]=gArea(trt)
  }
  
  treatment=ply_bf[ply_bf$Cumu_Area<= area & ply_bf$Cumu_Area!=0,]
  treatment=gUnionCascaded(treatment)
  
  df=area-gArea(treatment)
  nontr=ply_bf[ply_bf$Cumu_Area> area,]
  
  if (df>0){
    crds=gCentroid(nontr[1,])
    crds_bf=buffer(crds,width=sqrt(df/pi))
    treatment=gUnion(treatment,crds_bf)
  }
  
  ginter = gIntersection(treatment, ra_ply)
  if (class(ginter)[1]=="SpatialCollections"){
    ginter = ginter@polyobj
    ginter=gUnionCascaded(ginter)
  }
  
  trtN= gArea(ginter)/pixelArea
  inft = extract(inf, ginter, fun=sum, na.rm=T)
  
  ls = list(trtN, inft)
  
  return(ls)
  
}


######### Method 5  -- wavefront method weighted by hazard ########

wvfrt = function(inf){
  ## Derive wave front 
  
  library(concaveman)
  
  pts=rasterToPoints(inf,fun=function(x){x>0},spatial = T)
  
  crds=pts@coords
  concave=concaveman(crds,concavity = 1)
  p=Polygon(concave)
  ps=Polygons(list(p),1)
  sps=SpatialPolygons(list(ps))
  crs(sps)=crs(pts)
  sps=SpatialPolygonsDataFrame(sps,data=data.frame(ID=1))
  
  # lns is used as the wave front
  lns=as(sps,"SpatialLinesDataFrame")
  return(lns)
}

wvfrtHzd = function(inf, distance_classes, host,buffer,budget,  cost_per_meter_sq, a, rep, np, ncore){
  
  # get wavefront, lns is spatial Line
  lns=wvfrt(inf)
  pixelArea=xres(inf)*yres(inf)
  
  ra_ply=rasterToPolygons(inf,fun=function(x){x>0},dissolve = F)
  distance=gDistance(lns, ra_ply, byid=T)
  
  ra_ply$dis_frtwv=distance[1:length(ra_ply)]
  
  ## derive  ip ##
  host_uninf = host
  host_uninf[inf>0]=0
  
  
  inf_pts = rasterToPoints(inf,fun=function(x){x>0} ,spatial=T)
  names(inf_pts)="inf_level"
  n = length(inf_pts)
  wc_va = extract(Wcoef,inf_pts)
  inf_pts$inf_level = inf_pts$inf_level 
  
  host_pts = rasterToPoints(host_uninf, fun=function(x){x>0}, spatial=T)
  names(host_pts)="host"
  
  
  cl = makeCluster(ncore)
  registerDoParallel(cl) 
  
  ip_va = foreach(i=1:n, .combine=rbind, .packages = c('raster','rgdal','geosphere','rgeos'))%dopar%{
    dist= gDistance(inf_pts[i,], host_pts,byid=T)
    
    dist2 = 1/(dist^2 + a^2)
    dist2[dist>10000] = 0
    #dist2[dist>5000] = 0
    
    #infe_pot = (inf_pts$inf_level[i]*rep)^np*dist2*host_pts$host
    
    infe_pot = inf_pts$inf_level[i]*wc_va[i]*dist2*host_pts$host
    #infe_pot = (inf_pts$inf_level[i]*rep*wc_va[i])^np*dist2*host_pts$host
    
    infe_potS = sum(infe_pot)
    return(infe_potS)
  }
  
  stopCluster(cl)
  
  ra_ply$ip = as.vector(ip_va)
  ra_ply2=ra_ply[order(ra_ply$dis_frtwv,decreasing = F),]
  
  
  # based on the distance to wave front, classify all infested pixels/polygons into 
  # 6 classes using kmeans
  library(dplyr)
  centers <- kmeans(ra_ply2$dis_frtwv, centers = distance_classes)$centers
  # order the centers
  centers <- sort(centers)
  group <- kmeans(ra_ply2$dis_frtwv, centers = centers)$cluster
  ra_ply2$group=group
  
  rank = ra_ply2[1,]
  for (i in 1:distance_classes){
    sub = ra_ply2[ra_ply2$group==i,]
    sub2 =sub[order(sub$ip,decreasing = T),]
    rank=rbind(rank, sub2)
  }
  rank =rank[-1,]
  ra_ply2= rank
  
  ply_bf=buffer(ra_ply2,width=buffer,dissolve=F)
  
  area=budget/cost_per_meter_sq
  n=floor(area/pixelArea)+1
  ply_bf$Cumu_Area=0
  if (length(ply_bf)< n){n = length(ply_bf)}
  
  for (i in 1:n){
    trt=gUnionCascaded(ply_bf[1:i,])
    ply_bf$Cumu_Area[i]=gArea(trt)
  }
  
  
  ply_select=ply_bf[ply_bf$Cumu_Area< area & ply_bf$Cumu_Area!=0,]
  ply_Nonselect=ply_bf[ply_bf$Cumu_Area> area | ply_bf$Cumu_Area==0,]
  
  
  df= area - max(ply_select$Cumu_Area)
  
  for (i in 1:length(ply_Nonselect)){
    trt=gUnionCascaded(ply_Nonselect[1:i,])
    ply_Nonselect$Cumu_Area[i]=gArea(trt)
  }
  
  treatment=rbind(ply_select, ply_Nonselect[ply_Nonselect$Cumu_Area<=df,])
  treatment=gUnionCascaded(treatment)
  
  df=area-gArea(treatment)
  nontr=ply_Nonselect[ply_Nonselect$Cumu_Area>df,]
  
  if (df>0){
    crds=gCentroid(nontr[1,])
    crds_bf=buffer(crds,width=sqrt(df/pi))
    treatment=gUnion(treatment,crds_bf)
  }
  
  ginter = gIntersection(treatment, ra_ply)
  if (class(ginter)[1]=="SpatialCollections"){
    ginter = ginter@polyobj
    ginter=gUnionCascaded(ginter)
  }
  
  trtN= gArea(ginter)/pixelArea
  inft = extract(inf, ginter, fun=sum, na.rm=T)
  
  ls = list(trtN, inft)
  
  return(ls)
  
}

wvfrtRank = function(inf, distance_classes, host,buffer, a, ncore){
  
  lns=wvfrt(inf)
  pixelArea=xres(inf)*yres(inf)
  
  ra_ply=rasterToPolygons(inf,fun=function(x){x>0},dissolve = F)
  distance=gDistance(lns, ra_ply, byid=T)
  
  ra_ply$dis_frtwv=distance[1:length(ra_ply)]
  
  ## derive  ip ##
  host_uninf = host
  host_uninf[inf>0]=0
  
  
  inf_pts = rasterToPoints(inf,fun=function(x){x>0} ,spatial=T)
  names(inf_pts)="inf_level"
  n = length(inf_pts)
  wc_va = extract(Wcoef,inf_pts)
  inf_pts$inf_level = inf_pts$inf_level 
  
  host_pts = rasterToPoints(host_uninf, fun=function(x){x>0}, spatial=T)
  names(host_pts)="host"
  
  
  cl = makeCluster(ncore)
  registerDoParallel(cl) 
  
  ip_va = foreach(i=1:n, .combine=rbind, .packages = c('raster','rgdal','geosphere','rgeos'))%dopar%{
    dist= gDistance(inf_pts[i,], host_pts,byid=T)
    
    dist2 = 1/(dist^2 + a^2)
    dist2[dist>10000] = 0
    #dist2[dist>5000] = 0
    
    #infe_pot = (inf_pts$inf_level[i]*rep)^np*dist2*host_pts$host
    
    infe_pot = inf_pts$inf_level[i]*wc_va[i]*dist2*host_pts$host
    #infe_pot = (inf_pts$inf_level[i]*rep*wc_va[i])^np*dist2*host_pts$host
    
    infe_potS = sum(infe_pot)
    return(infe_potS)
  }
  
  stopCluster(cl)
  
  ra_ply$ip = as.vector(ip_va)
  ra_ply2=ra_ply[order(ra_ply$dis_frtwv,decreasing = F),]
  
  # based on the distance to wave front, classify all infested pixels/polygons into 
  # 6 classes using kmeans
  library(dplyr)
  centers <- kmeans(ra_ply2$dis_frtwv, centers = distance_classes)$centers
  # order the centers
  centers <- sort(centers)
  group <- kmeans(ra_ply2$dis_frtwv, centers = centers)$cluster
  ra_ply2$group=group
  
  rank = ra_ply2[1,]
  for (i in 1:distance_classes){
    sub = ra_ply2[ra_ply2$group==i,]
    sub2 =sub[order(sub$ip,decreasing = T),]
    rank=rbind(rank, sub2)
  }
  rank =rank[-1,]
  ra_ply2= rank
  
  return(ra_ply2)
}

#################### Method 6 --  infestation potential  #####################

## Step 1 -- rank each pixel based on infestation potential  ##
# inf: infestation raster; 
# host: host map;  
# np: number of step, this parameter doesn't really matter, but need to be a postive number
# a : natural dispersal scale
# rep: reproduction rate
# Wcoef: mean weather coefficient fraster or the simulated time period (mean for every year or the whole simulation period)
# range_buffer: a number for distance in meter, for a given infested pixel, only consider susceptible hosts within this range,
# this number should be a reasonable large number impacted by the natural dispersal rate
# ncore: number of cores used for calculation
ip_rank = function(inf, host, np, a, rep, Wcoef, range_buffer,ncore){
  
  # only counts susceptible hosts for the infestation potential 
  host_uninf = host
  host_uninf[inf>0]=0
  
  # covert infestation and host raster to points to get raster values and 
  # to easier calculate distance between infested pixel and susceptible host
  inf_pts = rasterToPoints(inf,fun=function(x){x>0} ,spatial=T)
  names(inf_pts)="inf_level"
  wc_va = extract(Wcoef,inf_pts)
  inf_pts$inf_level = inf_pts$inf_level 
  
  host_pts = rasterToPoints(host_uninf, fun=function(x){x>0}, spatial=T)
  names(host_pts)="host"
  host_pts$host = 1
  
  n = length(inf_pts)
  
  
  ip_fun = function(crds_list, pts_host){
    
    df = as.data.frame(matrix(crds_list, ncol=2))
    pts = SpatialPoints(df)
    
    tt = Sys.time()
    dist= gDistance(pts, host_pts,byid=T)
    Sys.time()-tt
    
    tt = Sys.time()
    dist= gDistance(inf_pts[1:300,], host_pts,byid=T)
    Sys.time()-tt
    
    dist2 = 1/(dist^2 + a^2)
    dist2[dist>range_buffer] = 0
    
    infe_pot = inf_pts$inf_level[i]*wc_va[i]*dist2*host_pts$host
    infe_potS = sum(infe_pot)
    
    return(infe_potS)
  }
  
  ip_fun = function(crds_list, pts_host){
    
    df = as.data.frame(matrix(crds_list, ncol=2))
    pts = SpatialPoints(df)
    
    tt = Sys.time()
    dist= gDistance(pts, host_pts,byid=T)
    Sys.time()-tt
    
    tt = Sys.time()
    dist= gDistance(inf_pts[1:300,], host_pts,byid=T)
    Sys.time()-tt
    
    dist2 = 1/(dist^2 + a^2)
    dist2[dist>range_buffer] = 0
    
    infe_pot = inf_pts$inf_level[i]*wc_va[i]*dist2*host_pts$host
    infe_potS = sum(infe_pot)
    
    return(infe_potS)
  }
  
  inf_pts2 = inf_pts[1:100,]
  
  t0 = Sys.time()
  crds_list <- split(inf_pts2@coords, seq(length(inf_pts2)))
  ts= sapply(crds_list, ip_fun,  pts_host = host_pts)
  Sys.time() - t0
  
  
  
  crds_list <- split(inf_pts2@coords, seq(length(inf_pts2)))
  
  list_all = list()
  nm = as.integer(length(inf_pts2)/ncore)
  
  
  for (i in 1:ncore){
    
    if (i==ncore){
      id1 = (i-1)*nm + 1
      id2 = length(inf_pts2)
      nm2 = length(inf_pts2[c(id1:id2),])
      crds_list <- split(inf_pts2@coords[id1:id2,1:2], seq(nm2))
      list_all[[i]] =  crds_list}
    else { 
      id1 = (i-1)*nm +1
      id2 = i*nm
      
      crds_list <- split(inf_pts2@coords[id1:id2,1:2], seq(nm))
      list_all[[i]] =  crds_list}
  }
  
  
  
  t0 = Sys.time()
  ## for each infested pixel, calculate the infestation potential for each susceptible host within the range_buffer
  ## and sum up
  n = 10
  t0 = Sys.time()
  cl = makeCluster(ncore)
  registerDoParallel(cl) 
  
  ip_va = foreach(i=1:ncore, .combine=rbind, .export=c("ip_fun"), .packages = c('raster','rgdal','geosphere','rgeos'))%dopar%{
    
    #dist= gDistance(inf_pts[i,], host_pts,byid=T)
    
    #dist2 = 1/(dist^2 + a^2)
    #dist2[dist>range_buffer] = 0
    
    
    # so the calculation of infestation potential is: 
    #(dispersal kernel)* (infestation level)* (reproduction rate) * (weather coefficient of the infested pixel) * (number of step) * (host abundence)
    #infe_pot = inf_pts$inf_level[i]*rep*wc_va[i]*np*dist2*host_pts$host
    #infe_pot = inf_pts$inf_level[i]*wc_va[i]*dist2*host_pts$host
    #infe_potS = sum(infe_pot)
    
    #return(infe_potS)
    
    ts= sapply(list_all[[i]], ip_fun,  pts_host = host_pts)
    
    return(ts)
  }
  
  
}


############## Old ip_rank function##############
ip_rank = function(inf, host, np, a, rep, Wcoef, range_buffer,ncore){
  
  # only counts susceptible hosts for the infestation potential 
  host_uninf = host
  host_uninf[inf>0]=0
  
  # covert infestation and host raster to points to get raster values and 
  # to easier calculate distance between infested pixel and susceptible host
  inf_pts = rasterToPoints(inf,fun=function(x){x>0} ,spatial=T)
  names(inf_pts)="inf_level"
  inf_pts$inf_level = inf_pts$inf_level 
  # extract weather_coefficient for each infested pixel/points          
  wc_va = extract(Wcoef,inf_pts)
  
  host_pts = rasterToPoints(host_uninf, fun=function(x){x>0}, spatial=T)
  names(host_pts)="host"
  
  n = length(inf_pts)
  
  ## for each infested pixel, calculate the infestation potential for each susceptible host within the range_buffer
  ## and sum up
  cl = makeCluster(ncore)
  registerDoParallel(cl) 
  
  ip_va = foreach(i=1:n, .combine=rbind, .export=c("host_pts","inf_pts"), .packages = c('raster','rgdal','geosphere','rgeos'))%dopar%{
    dist= gDistance(inf_pts[i,], host_pts,byid=T)
    
    dist2 = 1/(dist^2 + a^2)
    dist2[dist>range_buffer] = 0
    
    # so the calculation of infestation potential is: 
    #(dispersal kernel)* (infestation level)* (reproduction rate) * (weather coefficient of the infested pixel) * (number of step) * (host abundence)
    infe_pot = inf_pts$inf_level[i]*rep*wc_va[i]*np*dist2*host_pts$host
    
    
    infe_potS = sum(infe_pot)
    return(infe_potS)
  }
  
  stopCluster(cl)
  
  inf_pts$ip = as.vector(ip_va)
  inf_ply = rasterToPolygons(inf, fun=function(x){x>0},na.rm = T)
  inf_ply$ip = inf_pts$ip
  inf_ply2 = inf_ply[order(inf_ply$ip,decreasing = T),]
  #inf_pts2 = spTransform(inf_pts2, crs(inf))
  return(inf_ply2)
}



################ IP Rank no Dispersal Scale #########
ip_rank2 = function(inf, host, np, a, rep, Wcoef, range_buffer,ncore){
  
  # only counts susceptible hosts for the infestation potential 
  host_uninf = host
  host_uninf[inf>0]=0
  
  # covert infestation and host raster to points to get raster values and 
  # to easier calculate distance between infested pixel and susceptible host
  inf_pts = rasterToPoints(inf,fun=function(x){x>0} ,spatial=T)
  names(inf_pts)="inf_level"
  inf_pts$inf_level = inf_pts$inf_level 
  # extract weather_coefficient for each infested pixel/points          
  wc_va = extract(Wcoef,inf_pts)
  
  host_pts = rasterToPoints(host_uninf, fun=function(x){x>0}, spatial=T)
  names(host_pts)="host"
  
  n = length(inf_pts)
  
  ## for each infested pixel, calculate the infestation potential for each susceptible host within the range_buffer
  ## and sum up
  cl = makeCluster(ncore)
  registerDoParallel(cl) 
  
  ip_va = foreach(i=1:n, .combine=rbind, .export=c("host_pts","inf_pts"), .packages = c('raster','rgdal','geosphere','rgeos'))%dopar%{
    dist= gDistance(inf_pts[i,], host_pts,byid=T)
    
    dist2 = 1/(dist^2)
    dist2[dist>range_buffer] = 0
    
    # so the calculation of infestation potential is: 
    #(dispersal kernel)* (infestation level)* (reproduction rate) * (weather coefficient of the infested pixel) * (number of step) * (host abundence)
    infe_pot = inf_pts$inf_level[i]*rep*wc_va[i]*np*dist2*host_pts$host
    
    
    infe_potS = sum(infe_pot)
    return(infe_potS)
  }
  
  stopCluster(cl)
  
  inf_pts$ip = as.vector(ip_va)
  inf_ply = rasterToPolygons(inf, fun=function(x){x>0},na.rm = T)
  inf_ply$ip = inf_pts$ip
  inf_ply2 = inf_ply[order(inf_ply$ip,decreasing = T),]
  #inf_pts2 = spTransform(inf_pts2, crs(inf))
  return(inf_ply2)
}

# Step 2 -- select pixels for treatment based on rank of infestation potential #
# ip_rank_ply: return of the ip_rank function (polygon dataframe), each polygon in the polygon dataframe is one infested pixel
# the polygons are already ranked based on the infestation potential (ip) in a way that the first polgyon has the highest ip
# inf : infestation raster
# budget, cost_per_meter_sq : the same variables in your functions
# buffer: treatment buffer (in meter)
ip_treat = function(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf){
  
  area=budget/cost_per_meter_sq
  pixelArea=xres(inf)*yres(inf)
  
  
  ply_bf=buffer(ip_rank_ply,width=buffer,dissolve=F)
  n=floor(area/pixelArea)+2
  ply_bf$Cumu_Area=0
  
  if (length(ply_bf)< n){n = length(ply_bf)}
  
  # 
  for (i in 1:n){
    trt=gUnionCascaded(ply_bf[1:i,])
    ply_bf$Cumu_Area[i]=gArea(trt)
  }
  
  # select pixels together with the treatment buffer whoes total area is not larger than the budget allowed
  treatment=ply_bf[ply_bf$Cumu_Area<= area & ply_bf$Cumu_Area!=0,]
  treatment=gUnionCascaded(treatment)
  
  # if the selected treatment area is smaller than budget allowed, select part of the next infested pixel
  df=area-gArea(treatment)
  nontr=ply_bf[ply_bf$Cumu_Area> area,]
  
  if (df>0){
    crds=gCentroid(nontr[1,])
    crds_bf=buffer(crds,width=sqrt(df/pi))
    treatment=gUnion(treatment,crds_bf)
  }
  
  ginter = gIntersection(treatment, ip_rank_ply)
  if (class(ginter)[1]=="SpatialCollections"){
    ginter = ginter@polyobj
    ginter=gUnionCascaded(ginter)
  }
  
  trtN= gArea(ginter)/pixelArea
  inft = extract(inf, ginter, fun=sum, na.rm=T)
  
  ls = list(trtN, inft)
  
  return(ls)
  
}

