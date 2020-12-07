
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(PoPS)
library(abind)

setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/Prop10/")
setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/")

######## Prepare for 1st year #########

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
for (p in 3:3){
  
  reproductive_rate[[1]] = repros[p]
  natural_distance_scale[[1]] = disp[p]
  
  for (ld in 3:3){
    
    percent_natural_dispersal[[1]] = longdis[ld]
    
    inf_Rd=inf
    inf_Hinf=inf
    inf_Hst0=inf
    inf_Hst=inf
    inf_Hst2=inf
    inf_Ip=inf
    inf_Ip2=inf
    inf_Wvfrt1=inf
    inf_Wvfrt2=inf
    inf_No=inf
    
    infected_species_Rd = list()
    infected_species_No = list()
    infected_species_Hinf = list()
    infected_species_Hst0 = list()
    infected_species_Hst = list()
    infected_species_Hst2 = list()
    infected_species_Ip = list()
    infected_species_Ip2 = list()
    infected_species_Wvfrt1 = list()
    infected_species_Wvfrt2 = list()
    
    susceptible_species_Rd = list()
    susceptible_species_No = list()
    susceptible_species_Hinf = list()
    susceptible_species_Hst0 = list()
    susceptible_species_Hst = list()
    susceptible_species_Hst2 = list()
    susceptible_species_Ip = list()
    susceptible_species_Wvfrt1 = list()
    susceptible_species_Wvfrt2 = list()
    susceptible_species_Ip2 = list()
    
    r=100
    infected_species_Hinf[[1]] = as.matrix(inf_Ip)
    susceptible_species_Hinf[[1]] = as.matrix(host - inf_Hinf)
    
    
    for (i in 1:3){
      
      d = tempfile()
      dir.create(d)
      setwd(d)
      
      
      
      infected_species_Rd[[1]] = as.matrix(inf_Rd)
      infected_species_Hinf[[1]] = as.matrix(inf_Hinf)
      infected_species_Hst0[[1]] = as.matrix(inf_Hst0)
      infected_species_Hst[[1]] = as.matrix(inf_Hst)
      infected_species_Hst2[[1]] = as.matrix(inf_Hst2)
      infected_species_Ip[[1]] = as.matrix(inf_Ip)
      infected_species_Ip2[[1]] = as.matrix(inf_Ip2)
      infected_species_Wvfrt1[[1]] = as.matrix(inf_Wvfrt1)
      infected_species_Wvfrt2[[1]] = as.matrix(inf_Wvfrt2)
      infected_species_No[[1]] = as.matrix(inf_No)
      
      
      susceptible_species_Rd[[1]] <- as.matrix(host - inf_Rd)
      susceptible_species_Hinf[[1]] <- as.matrix(host - inf_Hinf)
      susceptible_species_Hst0[[1]] <- as.matrix(host - inf_Hst0)
      susceptible_species_Hst[[1]] <- as.matrix(host - inf_Hst)
      susceptible_species_Hst2[[1]] <- as.matrix(host - inf_Hst2)
      susceptible_species_Ip[[1]] <- as.matrix(host - inf_Ip)
      susceptible_species_Ip2[[1]] <- as.matrix(host - inf_Ip2)
      susceptible_species_Wvfrt1[[1]] <- as.matrix(host - inf_Wvfrt1)
      susceptible_species_Wvfrt2[[1]] <- as.matrix(host - inf_Wvfrt2)
      susceptible_species_No[[1]] <- as.matrix(host - inf_No)
      
      rate = data.frame(matrix(0,ncol=4))
      
      for (j in 1:20){
        random_seed= j*1000+50
        
        infected_species_Ip[[1]] = as.matrix(inf_Ip)
        susceptible_species_Ip[[1]] <- as.matrix(host - inf_Ip)
        
        dataIp <- tryCatch(pops_model(random_seed = random_seed, 
                                      use_lethal_temperature = use_lethal_temperature, 
                                      lethal_temperature = lethal_temperature, 
                                      lethal_temperature_month = lethal_temperature_month,
                                      
                                      use_movements=FALSE,
                                      movements=list(0,0,0,0,0),
                                      movements_dates= start_time,
                                      
                                      exposed=temperature[1:2],
                                      model_type_="SEI",
                                      latency_period=1,
                                      
                                      
                                      infected = infected_species_Ip[[1]],
                                      susceptible = susceptible_species_Ip[[1]],
                                      total_plants = total_pl[[1]],
                                      mortality_on = mortality_on,
                                      mortality_tracker = infected_species_Ip[[1]]*0,
                                      mortality = infected_species_Ip[[1]]*0,
                                      treatment_maps = no,
                                      
                                      treatment_dates = c("2019-03-01"),
                                      pesticide_duration=c(0),
                                      resistant = infected_species_Ip[[1]]*0,
                                      weather = weather,
                                      temperature = temperature,
                                      weather_coefficient = wc,
                                      ew_res = ew_res, ns_res = ns_res, num_rows = num_rows, num_cols = num_cols,
                                      time_step = time_step, reproductive_rate = reproductive_rate[[1]],
                                      mortality_rate = mortality_rate, mortality_time_lag = mortality_time_lag,
                                      season_month_start = season_month_start, season_month_end = season_month_end,
                                      start_date = start_time, end_date = end_time,
                                      treatment_method = "all infected", 
                                      
                                      natural_kernel_type = natural_kernel_type[[1]], anthropogenic_kernel_type = anthropogenic_kernel_type[[1]], 
                                      use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = 1,
                                      natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                                      natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                                      anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        )
        
        
        
        rate[j,1:4] = dataIp$rates[[1]]
        
        print(j)
        print(dataIp$rates)
        
      }
      
      #rate[is.na(rate)]=0
      # me = colMeans(rate, na.rm=T)
      
      md=c()
      for (k in 1:4){
        md = c(md,median(rate[,k],na.rm=T))
      }
      #median(rates,na.rm=T)
      rg0= mean(md,na.rm=T)
      if (is.na(rg0)){rg=rg}
      else if (rg0<rg){rg0=rg}
      else {rg = rg0}
      
      
      if (rg==0){rg=2000}
      
      rd= random_pixel(inf_Rd,buffer,budget,cost_per_meter_sq)
      print("RDdone")
      hinf = Hinfest_pixel(inf_Hinf,buffer,budget,cost_per_meter_sq)
      print("Hinfdone")
      hst =threat_pixel(inf_Hst,width=2000,host=host,budget,buffer,cost_per_meter_sq) #30000
      print("Hstdone")
      
      
      wvfrt1=wvfrt_pixel(inf_Wvfrt1,buffer,budget,cost_per_meter_sq)
      print("wvfrt1done")
      wvfrt2=wvfrtHzd(inf_Wvfrt2, 10,host,buffer,budget,cost_per_meter_sq, a=natural_distance_scale[[1]], rep=reproductive_rate[[1]], np=8,20)
      print("wvfrt2done")
      ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,2000 ,20)
      ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)
      print("IPdone")
      
      
      
    }
    print(Sys.time()-t0)
    
    
    
    dfsm[1:2,]
    
    
    # 10:19 ~ 