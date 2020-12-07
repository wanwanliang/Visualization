
setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/co/")

ply=ra2plyMerge(inf)
plys=ply
plys_Rd=plys
plys_Hinf=plys
plys_Hst0=plys
plys_Hst=plys
plys_Hst2=plys
plys_Ip=plys
plys_Wvfrt1=plys
plys_Wvfrt2=plys
plys_No=plys

buffer=150
pixelArea=10000
budget=2000000
cost_per_meter_sq=1
cost_per_meter_sq = 1
treatment_years=c(2019)

start_time <- "2019-01-01"
end_time <- "2019-12-30"



no=list(as.matrix(inf*0))
df1=as.data.frame(matrix(0,nrow=10,ncol=20))
dfsm=as.data.frame(matrix(0,nrow=4,ncol=20))
colnames(df1)=c("Random","Hinf","Hst0","Hst1", "Hst2" ,"Ip","Wvfrt1","Wvfrt2","Notrt","Ip2","Random","Hinf","Hst0","Hst1", "Hst2" ,"Ip","Wvfrt1","Wvfrt2","Notrt")
colnames(dfsm)=c("Random","Hinf","Hst0","Hst1", "Hst2" ,"Ip","Wvfrt1","Wvfrt2","Notrt","Ip2","Random","Hinf","Hst0","Hst1", "Hst2" ,"Ip","Wvfrt1","Wvfrt2","Notrt")

# 3 sets parameters #
repros = c(4, 8, 16) 
disp = c(10, 20, 40) 

repros = c(2, 4, 8, 16) 
disp = c(5, 10, 20, 40)
longdis = c(0.99, 0.97, 0.95) 

reproductive_rate[[1]] = repros[2]
natural_distance_scale[[1]] = disp[2]
percent_natural_dispersal[[1]] = longdis[2]

anthropogenic_distance_scale[[1]] = 8000 
use_anthropogenic_kernel = TRUE
cos
t_per_meter_sq = 1
dim1 = dim(inf)[1]
dim2 = dim(inf)[2]
r=20 # rep

######## Run simulations for each host type ##########
rg=0
t0 = Sys.time()
inf_Rd=inf
inf_Hinf=inf
inf_Hst=inf
inf_Ip=inf
inf_Wvfrt1=inf
inf_Wvfrt2=inf
inf_No=inf


# host 
ra_ply_Hst=rasterToPolygons(inf_Hst,fun=function(x){x>0},dissolve = F)
hst_ply_rank = Thost(inf_Hst, ra_ply, width=2000, host = host)
hst_ply_rank$Thst
hst_ply_rank=hst_ply_rank[order(hst_ply_rank$Thst,decreasing = T),]
hst_ply_rank
writeOGR(hst_ply_rank, ".", "hstY1Ply",driver = "ESRI Shapefile")

# hinf
ra_ply_hinf=rasterToPolygons(inf_Hinf,fun=function(x){x>0},dissolve = F)
names(ra_ply_hinf) = "infest_level"
ra_ply_hinf=ra_ply_hinf[order(ra_ply_hinf$infest_level,decreasing = T),]
writeOGR(ra_ply_hinf, ".", "hinfY1Ply",driver = "ESRI Shapefile")

#ip 
ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,2000 ,20)
writeOGR(ip_rank_ply, ".", "ipY1Ply",driver = "ESRI Shapefile")

# wvfrt1
lns=wvfrt(inf_Wvfrt1)
ra_ply_wvfrt1=rasterToPolygons(inf_Wvfrt1,fun=function(x){x>0},dissolve = F)
distance=gDistance(lns, ra_ply_wvfrt1, byid=T)

ra_ply_wvfrt1$dis_frtwv=distance[1:length(ra_ply_wvfrt1)]
ra_ply_wvfrt1=ra_ply_wvfrt1[order(ra_ply_wvfrt1$dis_frtwv,decreasing = F),]
writeOGR(ra_ply_wvfrt1, ".", "wvfrt1Y1Ply",driver = "ESRI Shapefile")

#wvfrt2
ra_ply_wvfrt2 = wvfrtRank(inf_Wvfrt2, 10, host, 0, natural_distance_scale[[1]],20)
writeOGR(ra_ply_wvfrt2, ".", "wvfrt2Y1Ply",driver = "ESRI Shapefile")

