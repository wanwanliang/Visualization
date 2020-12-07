
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
      hst0 = threat_pixel(inf_Hst,width=rg,host=host,budget,buffer,cost_per_meter_sq) 
      hst =threat_pixel(inf_Hst,width=2000,host=host,budget,buffer,cost_per_meter_sq) #30000
      print("Hstdone")
      hst2 = treat_m6(inf_Hst2,natural_distance_scale[[1]],reproductive_rate[[1]],Nstep=8,Wcoef,host, budget, buffer,cost_per_meter_sq)
      print("Hst2done")
      
      wvfrt1=wvfrt_pixel(inf_Wvfrt1,buffer,budget,cost_per_meter_sq)
      print("wvfrt1done")
      wvfrt2=wvfrtHzd(inf_Wvfrt2, 10,host,buffer,budget,cost_per_meter_sq, a=natural_distance_scale[[1]], rep=reproductive_rate[[1]], np=8,20)
      print("wvfrt2done")
      ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,rg ,20)
      ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)
      print("IPdone")
      
      ip_rank_ply2 = ip_rank2(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,rg ,20)
      ip2b = ip_treat(ip_rank_ply2, budget, buffer, cost_per_meter_sq, inf)
      print("IP2done")
      
      no_ls = stack()
      rd_ls = stack()
      ip_ls = stack()
      ip_ls2 = stack()
      hinf_ls = stack()
      hst0_ls = stack()
      hst_ls = stack()
      hst2_ls = stack()
      wvfrt1_ls = stack()
      wvfrt2_ls = stack()
      
      treatment_years=c(2019)
      start_time <- "2019-03-01"
      end_time <- "2019-12-30"
      season_month_start = 3
      season_month_end = 10
      #id1 = (i-1)*8 + 1
      #id2 = i*8
      #wc = Wcoef[[id1:id2]]
      
      
      for (j in 1:r){
        random_seed= j*1000+50
        
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
        
        
        dataNo <- pops_model(random_seed = random_seed, 
                             use_lethal_temperature = use_lethal_temperature, 
                             lethal_temperature = lethal_temperature, 
                             lethal_temperature_month = lethal_temperature_month,
                             
                             infected = infected_species_No[[1]],
                             susceptible = susceptible_species_No[[1]],
                             total_plants = total_pl[[1]],
                             mortality_on = mortality_on,
                             mortality_tracker = infected_species_No[[1]]*0,
                             mortality = infected_species_No[[1]]*0,
                             treatment_maps = no,
                             
                             treatment_dates = c("2019-03-01"),
                             pesticide_duration=c(0),
                             resistant = infected_species_No[[1]]*0,
                             weather = weather,
                             temperature = temperature,
                             weather_coefficient = wc,
                             ew_res = ew_res, ns_res = ns_res, num_rows = num_rows, num_cols = num_cols,
                             time_step = time_step, reproductive_rate = reproductive_rate[[1]],
                             mortality_rate = mortality_rate, mortality_time_lag = mortality_time_lag,
                             season_month_start = season_month_start, season_month_end = season_month_end,
                             start_date = start_time, end_date = end_time,
                             treatment_method = "all infected", 
                             
                             use_movements=FALSE,
                             movements=list(0,0,0,0,0),
                             movements_dates= start_time,
                             
                             exposed=temperature[1:2],
                             model_type_="SEI",
                             latency_period=1,
                             
                             natural_kernel_type = natural_kernel_type[[1]], anthropogenic_kernel_type = anthropogenic_kernel_type[[1]], 
                             use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                             natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                             natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                             anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        
        dataRd <- pops_model(random_seed = random_seed, 
                             use_lethal_temperature = use_lethal_temperature, 
                             lethal_temperature = lethal_temperature, 
                             lethal_temperature_month = lethal_temperature_month,
                             infected = infected_species_Rd[[1]],
                             susceptible = susceptible_species_Rd[[1]],
                             total_plants = total_pl[[1]],
                             mortality_on = mortality_on,
                             mortality_tracker = infected_species_Rd[[1]]*0,
                             mortality = infected_species_Rd[[1]]*0,
                             treatment_maps = rd,
                             treatment_dates = c("2019-03-01"),
                             pesticide_duration=c(0),
                             resistant = infected_species_Rd[[1]]*0,
                             
                             weather = weather,
                             temperature = temperature,
                             weather_coefficient = wc,
                             
                             use_movements=FALSE,
                             movements=list(0,0,0,0,0),
                             movements_dates= start_time,
                             
                             exposed=temperature[1:2],
                             model_type_="SEI",
                             latency_period=1,
                             
                             
                             
                             ew_res = ew_res, ns_res = ns_res, num_rows = num_rows, num_cols = num_cols,
                             time_step = time_step, reproductive_rate = reproductive_rate[[1]],
                             mortality_rate = mortality_rate, mortality_time_lag = mortality_time_lag,
                             season_month_start = season_month_start, season_month_end = season_month_end,
                             start_date = start_time, end_date = end_time,
                             treatment_method = "all infected", 
                             natural_kernel_type = natural_kernel_type[[1]], anthropogenic_kernel_type = anthropogenic_kernel_type[[1]], 
                             use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                             natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                             natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                             anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        
        dataHinf <- pops_model(random_seed = random_seed, 
                               use_lethal_temperature = use_lethal_temperature, 
                               lethal_temperature = lethal_temperature, 
                               lethal_temperature_month = lethal_temperature_month,
                               
                               use_movements=FALSE,
                               movements=list(0,0,0,0,0),
                               movements_dates= start_time,
                               
                               exposed=temperature[1:2],
                               model_type_="SEI",
                               latency_period=1,
                               
                               
                               infected = infected_species_Hinf[[1]],
                               susceptible = susceptible_species_Hinf[[1]],
                               total_plants = total_pl[[1]],
                               mortality_on = mortality_on,
                               mortality_tracker = infected_species_Hinf[[1]]*0,
                               mortality = infected_species_Hinf[[1]]*0,
                               treatment_maps = hinf,
                               
                               treatment_dates = c("2019-03-01"),
                               pesticide_duration=c(0),
                               resistant = infected_species_Hinf[[1]]*0,
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
                               use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                               natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                               natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                               anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        
        dataHst0 <- pops_model(random_seed = random_seed, 
                               use_lethal_temperature = use_lethal_temperature, 
                               lethal_temperature = lethal_temperature, 
                               lethal_temperature_month = lethal_temperature_month,
                               
                               infected = infected_species_Hst0[[1]],
                               susceptible = susceptible_species_Hst0[[1]],
                               total_plants = total_pl[[1]],
                               mortality_on = mortality_on,
                               mortality_tracker = infected_species_Hst0[[1]]*0,
                               mortality = infected_species_Hst0[[1]]*0,
                               treatment_maps = hst0,
                               
                               
                               use_movements=FALSE,
                               movements=list(0,0,0,0,0),
                               movements_dates= start_time,
                               
                               exposed=temperature[1:2],
                               model_type_="SEI",
                               latency_period=1,
                               
                               
                               treatment_dates = c("2019-03-01"),
                               pesticide_duration=c(0),
                               resistant = infected_species_Hst[[1]]*0,
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
                               use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                               natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                               natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                               anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        
        dataHst <- pops_model(random_seed = random_seed, 
                              use_lethal_temperature = use_lethal_temperature, 
                              lethal_temperature = lethal_temperature, 
                              lethal_temperature_month = lethal_temperature_month,
                              
                              infected = infected_species_Hst[[1]],
                              susceptible = susceptible_species_Hst[[1]],
                              total_plants = total_pl[[1]],
                              mortality_on = mortality_on,
                              mortality_tracker = infected_species_Hst[[1]]*0,
                              mortality = infected_species_Hst[[1]]*0,
                              treatment_maps = hst,
                              
                              
                              use_movements=FALSE,
                              movements=list(0,0,0,0,0),
                              movements_dates= start_time,
                              
                              exposed=temperature[1:2],
                              model_type_="SEI",
                              latency_period=1,
                              
                              
                              treatment_dates = c("2019-03-01"),
                              pesticide_duration=c(0),
                              resistant = infected_species_Hst[[1]]*0,
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
                              use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                              natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                              natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                              anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        dataHst2 <- pops_model(random_seed = random_seed, 
                               use_lethal_temperature = use_lethal_temperature, 
                               lethal_temperature = lethal_temperature, 
                               lethal_temperature_month = lethal_temperature_month,
                               
                               infected = infected_species_Hst2[[1]],
                               susceptible = susceptible_species_Hst2[[1]],
                               total_plants = total_pl[[1]],
                               mortality_on = mortality_on,
                               mortality_tracker = infected_species_Hst2[[1]]*0,
                               mortality = infected_species_Hst2[[1]]*0,
                               treatment_maps = hst2,
                               
                               
                               use_movements=FALSE,
                               movements=list(0,0,0,0,0),
                               movements_dates= start_time,
                               
                               exposed=temperature[1:2],
                               model_type_="SEI",
                               latency_period=1,
                               
                               
                               treatment_dates = c("2019-03-01"),
                               pesticide_duration=c(0),
                               resistant = infected_species_Hst2[[1]]*0,
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
                               use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                               natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                               natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                               anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        
        dataIp <- pops_model(random_seed = random_seed, 
                             use_lethal_temperature = use_lethal_temperature, 
                             lethal_temperature = lethal_temperature, 
                             lethal_temperature_month = lethal_temperature_month,
                             infected = infected_species_Ip[[1]],
                             susceptible = susceptible_species_Ip[[1]],
                             total_plants = total_pl[[1]],
                             mortality_on = mortality_on,
                             mortality_tracker = infected_species_Ip[[1]]*0,
                             mortality = infected_species_Ip[[1]]*0,
                             treatment_maps = ip2,
                             treatment_dates = c("2019-03-01"),
                             pesticide_duration=c(0),
                             resistant = infected_species_Ip[[1]]*0,
                             weather = weather,
                             temperature = temperature,
                             weather_coefficient = wc,
                             
                             use_movements=FALSE,
                             movements=list(0,0,0,0,0),
                             movements_dates= start_time,
                             
                             
                             exposed=temperature[1:2],
                             model_type_="SEI",
                             latency_period=1,
                             
                             
                             ew_res = ew_res, ns_res = ns_res, num_rows = num_rows, num_cols = num_cols,
                             time_step = time_step, reproductive_rate = reproductive_rate[[1]],
                             mortality_rate = mortality_rate, mortality_time_lag = mortality_time_lag,
                             season_month_start = season_month_start, season_month_end = season_month_end,
                             start_date = start_time, end_date = end_time,
                             treatment_method = "all infected", 
                             natural_kernel_type = natural_kernel_type[[1]], anthropogenic_kernel_type = anthropogenic_kernel_type[[1]], 
                             use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                             natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                             natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                             anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        dataIp2 <- pops_model(random_seed = random_seed, 
                              use_lethal_temperature = use_lethal_temperature, 
                              lethal_temperature = lethal_temperature, 
                              lethal_temperature_month = lethal_temperature_month,
                              infected = infected_species_Ip[[1]],
                              susceptible = susceptible_species_Ip[[1]],
                              total_plants = total_pl[[1]],
                              mortality_on = mortality_on,
                              mortality_tracker = infected_species_Ip[[1]]*0,
                              mortality = infected_species_Ip[[1]]*0,
                              treatment_maps = ip2b,
                              treatment_dates = c("2019-03-01"),
                              pesticide_duration=c(0),
                              resistant = infected_species_Ip[[1]]*0,
                              weather = weather,
                              temperature = temperature,
                              weather_coefficient = wc,
                              
                              use_movements=FALSE,
                              movements=list(0,0,0,0,0),
                              movements_dates= start_time,
                              
                              
                              exposed=temperature[1:2],
                              model_type_="SEI",
                              latency_period=1,
                              
                              
                              ew_res = ew_res, ns_res = ns_res, num_rows = num_rows, num_cols = num_cols,
                              time_step = time_step, reproductive_rate = reproductive_rate[[1]],
                              mortality_rate = mortality_rate, mortality_time_lag = mortality_time_lag,
                              season_month_start = season_month_start, season_month_end = season_month_end,
                              start_date = start_time, end_date = end_time,
                              treatment_method = "all infected", 
                              natural_kernel_type = natural_kernel_type[[1]], anthropogenic_kernel_type = anthropogenic_kernel_type[[1]], 
                              use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                              natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                              natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                              anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        
        dataWvfrt1 <- pops_model(random_seed = random_seed, 
                                 use_lethal_temperature = use_lethal_temperature, 
                                 lethal_temperature = lethal_temperature, 
                                 lethal_temperature_month = lethal_temperature_month,
                                 
                                 infected = infected_species_Wvfrt1[[1]],
                                 susceptible = susceptible_species_Wvfrt1[[1]],
                                 total_plants = total_pl[[1]],
                                 mortality_on = mortality_on,
                                 mortality_tracker = infected_species_Wvfrt1[[1]]*0,
                                 mortality = infected_species_Wvfrt1[[1]]*0,
                                 treatment_maps = wvfrt1,
                                 
                                 use_movements=FALSE,
                                 movements=list(0,0,0,0,0),
                                 movements_dates= start_time,
                                 
                                 exposed=temperature[1:2],
                                 model_type_="SEI",
                                 latency_period=1,
                                 
                                 
                                 treatment_dates = c("2019-03-01"),
                                 pesticide_duration=c(0),
                                 resistant = infected_species_Wvfrt1[[1]]*0,
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
                                 use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                                 natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                                 natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                                 anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        dataWvfrt2 <- pops_model(random_seed = random_seed, 
                                 use_lethal_temperature = use_lethal_temperature, 
                                 lethal_temperature = lethal_temperature, 
                                 lethal_temperature_month = lethal_temperature_month,
                                 
                                 infected = infected_species_Wvfrt2[[1]],
                                 susceptible = susceptible_species_Wvfrt2[[1]],
                                 total_plants = total_pl[[1]],
                                 mortality_on = mortality_on,
                                 mortality_tracker = infected_species_Wvfrt2[[1]]*0,
                                 mortality = infected_species_Wvfrt2[[1]]*0,
                                 treatment_maps = wvfrt2,
                                 
                                 exposed=temperature[1:2],
                                 model_type_="SEI",
                                 latency_period=1,
                                 
                                 
                                 use_movements=FALSE,
                                 movements=list(0,0,0,0,0),
                                 movements_dates= start_time,
                                 
                                 treatment_dates = c("2019-03-01"),
                                 pesticide_duration=c(0),
                                 resistant = infected_species_Wvfrt2[[1]]*0,
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
                                 use_anthropogenic_kernel = use_anthropogenic_kernel, percent_natural_dispersal = percent_natural_dispersal[[1]],
                                 natural_distance_scale = natural_distance_scale[[1]], anthropogenic_distance_scale = anthropogenic_distance_scale[[1]], 
                                 natural_dir = natural_dir[[1]], natural_kappa = natural_kappa[[1]],
                                 anthropogenic_dir = anthropogenic_dir[[1]], anthropogenic_kappa = anthropogenic_kappa[[1]],output_frequency = "year")
        
        
        
        df1[j,1]=dataRd$area_infected[1]/pixelArea
        df1[j,2]=dataHinf$area_infected[1]/pixelArea
        df1[j,3]=dataHst0$area_infected[1]/pixelArea
        df1[j,4]=dataHst$area_infected[1]/pixelArea
        df1[j,5]=dataHst2$area_infected[1]/pixelArea
        df1[j,6]=dataIp$area_infected[1]/pixelArea
        df1[j,7]=dataWvfrt1$area_infected[1]/pixelArea
        df1[j,8]=dataWvfrt2$area_infected[1]/pixelArea
        df1[j,9]=dataNo$area_infected[1]/pixelArea
        df1[j,10]=dataIp2$area_infected[1]/pixelArea
        
        df1[j,11]=dataRd$number_infected[1]
        df1[j,12]=dataHinf$number_infected[1]
        df1[j,13]=dataHst0$number_infected[1]
        df1[j,14]=dataHst$number_infected[1]
        df1[j,15]=dataHst2$number_infected[1]
        df1[j,16]=dataIp$number_infected[1]
        df1[j,17]=dataWvfrt1$number_infected[1]
        df1[j,18]=dataWvfrt2$number_infected[1]
        df1[j,19]=dataNo$number_infected[1]
        df1[j,20]=dataIp$number_infected[1]
        
        
        no_re=dataNo$infected
        nl = length(no_re)
        no_re=as.matrix(no_re[[nl]])
        no_re = raster(no_re, template = inf)
        no_ls= stack(no_ls,no_re)
        
        rd_re=dataRd$infected
        rd_re=as.matrix(rd_re[[nl]])
        rd_re = raster(rd_re, template = inf)
        rd_ls = stack(rd_ls,rd_re)
        
        
        hinf_re=dataHinf$infected
        hinf_re=as.matrix(hinf_re[[nl]])
        hinf_re = raster(hinf_re, template = inf)
        hinf_ls= stack(hinf_ls,hinf_re)
        
        hst0_re=dataHst0$infected
        hst0_re=as.matrix(hst0_re[[nl]])
        hst0_re=raster(hst0_re, template = inf)
        hst0_ls= stack(hst0_ls,hst0_re)
        
        
        hst_re=dataHst$infected
        hst_re=as.matrix(hst_re[[nl]])
        hst_re=raster(hst_re, template = inf)
        hst_ls= stack(hst_ls,hst_re)
        
        hst2_re=dataHst2$infected
        hst2_re=as.matrix(hst2_re[[nl]])
        hst2_re=raster(hst2_re, template = inf)
        hst2_ls= stack(hst2_ls,hst2_re)
        
        ip_re=dataIp$infected
        ip_re=as.matrix(ip_re[[nl]])
        ip_re=raster(ip_re, template = inf)
        ip_ls= stack(ip_ls,ip_re)
        
        ip_re2=dataIp2$infected
        ip_re2=as.matrix(ip_re2[[nl]])
        ip_re2=raster(ip_re2, template = inf)
        ip_ls2= stack(ip_ls2,ip_re2)
        
        wvfrt1_re=dataWvfrt1$infected
        wvfrt1_re=as.matrix(wvfrt1_re[[nl]])
        wvfrt1_re=raster(wvfrt1_re, template = inf)
        wvfrt1_ls= stack(wvfrt1_ls,wvfrt1_re)
        
        wvfrt2_re=dataWvfrt2$infected
        wvfrt2_re=as.matrix(wvfrt2_re[[nl]])
        wvfrt2_re=raster(wvfrt2_re, template = inf)
        wvfrt2_ls= stack(wvfrt2_ls,wvfrt2_re)
        
        print(j)
      }
      
      print (paste("Year",as.character(i)))
      
      
      mdv = lapply(1:10, function(x){with(df1, which.min(abs(df1[,x]- median(df1[,x]))))})
      
      
      inf_Rd = raster(rd_ls, layer = mdv[[1]])
      inf_Hinf = raster(hinf_ls, layer = mdv[[2]])
      inf_Hst0 = raster(hst0_ls, layer = mdv[[3]])
      inf_Hst = raster(hst_ls, layer = mdv[[4]])
      inf_Hst2 = raster(hst2_ls, layer = mdv[[5]])
      inf_Ip = raster(ip_ls, layer = mdv[[6]])
      inf_Wvfrt1 = raster(wvfrt1_ls, layer = mdv[[7]])
      inf_Wvfrt2 = raster(wvfrt2_ls, layer = mdv[[8]])
      inf_No = raster(no_ls, layer = mdv[[9]])
      inf_Ip2 = raster(ip_ls2, layer = mdv[[10]])
      
      
      area_rd = df1[mdv[[1]],1]
      area_hinf = df1[mdv[[2]],2]
      area_hst0 =  df1[mdv[[3]],3]
      area_hst =  df1[mdv[[4]],4]
      area_hst2 =  df1[mdv[[5]],5]
      area_ip =  df1[mdv[[6]],6]
      area_wvfrt1 =  df1[mdv[[7]],7]
      area_wvfrt2 =  df1[mdv[[8]],8]
      area_no =  df1[mdv[[9]],9]
      area_ip2 =  df1[mdv[[10]],10]
      
      num_rd = df1[mdv[[1]],10]
      num_hinf = df1[mdv[[2]],11]
      num_hst0 = df1[mdv[[3]],12]
      num_hst = df1[mdv[[4]],13]
      num_hst2 = df1[mdv[[5]],14]
      num_ip = df1[mdv[[6]],15]
      num_wvfrt1 = df1[mdv[[7]],16]
      num_wvfrt2 = df1[mdv[[8]],17]
      num_no = df1[mdv[[9]],18]
      num_ip2 = df1[mdv[[9]],20]
      
      
      dfsm[i,1:20]=c(area_rd, area_hinf, area_hst0, area_hst,area_hst2, area_ip,area_wvfrt1, area_wvfrt2, area_no,area_ip2, 
                     num_rd, num_hinf,num_hst0, num_hst, num_hst2, num_ip, num_wvfrt1, num_wvfrt2, num_no, num_ip2)
      print(dfsm[i,1:10])
      
      
      
   
      
      host_nm = substr(host_file, 1, nchar(host_file)-4)  
      loop_nm = paste("Disp", as.character(p-1), "Ldis", as.character(ld), "Year", as.character(i),sep="")
      
      no_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "No.tif",sep="")
      rd_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Rd.tif",sep="")
      hinf_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Hinf.tif",sep="")
      hst0_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Hst0.tif",sep="")
      hst_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Hst.tif",sep="")
      hst2_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Hst2.tif",sep="")
      ip_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Ip.tif",sep="")
      wvfrt1_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Wvfrt1.tif",sep="")
      wvfrt2_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Wvfrt2.tif",sep="")
      ip_nm2 = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/",host_nm, loop_nm, "Ip2.tif",sep="")
      
      writeRaster(inf_No, no_nm, overwrite=T)
      writeRaster(inf_Rd, rd_nm, overwrite=T)
      writeRaster(inf_Hinf, hinf_nm, overwrite=T)
      writeRaster(inf_Hst0, hst0_nm, overwrite=T)
      writeRaster(inf_Hst, hst_nm, overwrite=T)
      writeRaster(inf_Hst2, hst2_nm, overwrite=T)
      writeRaster(inf_Ip, ip_nm, overwrite=T)
      writeRaster(inf_Ip2, ip_nm2, overwrite=T)
      writeRaster(inf_Wvfrt1, wvfrt1_nm, overwrite=T)
      writeRaster(inf_Wvfrt2, wvfrt2_nm, overwrite=T)
      
      file.remove(dir(d, full.names = T))
    }
    
    dfnm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/Prop03/result/","Disp", as.character(p), "Ldis", as.character(ld),"Y", as.character(i),".csv" ,sep="")
    #dfsm$Year=c(1:3)
    #write.csv(dfsm,dfnm)
    
  }
  
}
print(Sys.time()-t0)



dfsm[1:2,]


# 10:19 ~ 