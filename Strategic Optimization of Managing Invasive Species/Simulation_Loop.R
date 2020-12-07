
library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(PoPS)
library(abind)

setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles")

######## Prepare for 1st year #########

ply=ra2plyMerge(inf)
plys=ply
plys_Rd=plys
plys_Hinf=plys
plys_Hst=plys
plys_Ip=plys
plys_Wvfrt1=plys
plys_Wvfrt2=plys
plys_No=plys

buffer=150
pixelArea=10000
budget=3000000
cost_per_meter_sq=1
treatment_years=c(2019)

start_time <- "2019-03-01"
end_time <- "2019-12-30"



inf_Rd=inf
inf_Hinf=inf
inf_Hst=inf
inf_Ip=inf
inf_Wvfrt1=inf
inf_Wvfrt2=inf
inf_No=inf

infected_species_Rd = list()
infected_species_No = list()
infected_species_Hinf = list()
infected_species_Hst = list()
infected_species_Ip = list()
infected_species_Wvfrt1 = list()
infected_species_Wvfrt2 = list()

susceptible_species_Rd = list()
susceptible_species_No = list()
susceptible_species_Hinf = list()
susceptible_species_Hst = list()
susceptible_species_Ip = list()
susceptible_species_Wvfrt1 = list()
susceptible_species_Wvfrt2 = list()


no=list(as.matrix(inf*0))
df1=as.data.frame(matrix(0,nrow=10,ncol=14))
dfsm=as.data.frame(matrix(0,nrow=4,ncol=14))
colnames(df1)=c("Random","Hinf","Hzd1","Hzd2","Wvfrt1","Wvfrt2","Notrt","Random","Hinf","Hzd1","Hzd2","Wvfrt1","Wvfrt2","Notrt")
colnames(dfsm)=c("Random","Hinf","Hzd1","Hzd2","Wvfrt1","Wvfrt2","Notrt","Random","Hinf","Hzd1","Hzd2","Wvfrt1","Wvfrt2","Notrt")

# 3 sets parameters #
repros = c(4, 8, 16) 
disp = c(10, 20, 40)  
longdis = c(0.99, 0.97, 0.95) 

anthropogenic_distance_scale[[1]] = 8000 
use_anthropogenic_kernel = TRUE

dim1 = dim(inf)[1]
dim2 = dim(inf)[2]
r=100 # rep

######## Run simulations for each host type ##########

t0 = Sys.time()
for (p in 1:3){
  
  reproductive_rate[[1]] = repros[p]
  natural_distance_scale[[1]] = disp[p]
  
  
  for (ld in 1:3){
    
    percent_natural_dispersal[[1]] = longdis[ld]
    
    for (i in 1:4){
      
      infected_species_Rd[[1]] = as.matrix(inf_Rd)
      infected_species_Hinf[[1]] = as.matrix(inf_Hinf)
      infected_species_Hst[[1]] = as.matrix(inf_Hst)
      infected_species_Ip[[1]] = as.matrix(inf_Ip)
      infected_species_Wvfrt1[[1]] = as.matrix(inf_Wvfrt1)
      infected_species_Wvfrt2[[1]] = as.matrix(inf_Wvfrt2)
      infected_species_No[[1]] = as.matrix(inf_No)
      
      
      susceptible_species_Rd[[1]] <- as.matrix(host - inf_Rd)
      susceptible_species_Hinf[[1]] <- as.matrix(host - inf_Hinf)
      susceptible_species_Hst[[1]] <- as.matrix(host - inf_Hst)
      susceptible_species_Ip[[1]] <- as.matrix(host - inf_Ip)
      susceptible_species_Wvfrt1[[1]] <- as.matrix(host - inf_Wvfrt1)
      susceptible_species_Wvfrt2[[1]] <- as.matrix(host - inf_Wvfrt2)
      susceptible_species_No[[1]] <- as.matrix(host - inf_No)
      
      
      rd= random_pixel(inf_Rd,buffer,budget,cost_per_meter_sq)
      hinf = Hinfest_pixel(inf_Hinf,buffer,budget,cost_per_meter_sq)
      hst=threat_pixel(inf_Hst,width=2000,plys_Hst,host=host,budget,buffer,cost_per_meter_sq,10)
      wvfrt1=wvfrt_pixel(inf_Wvfrt1,buffer,budget,cost_per_meter_sq)
      wvfrt2=wvfrtHzd(inf_Wvfrt2, 10,host,buffer,budget,cost_per_meter_sq, a=natural_distance_scale[[1]], rep=reproductive_rate[[1]], np=8,10)
      ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,10000 ,10)
      ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)
      
      
      
      no_ls = list()
      rd_ls = list()
      ip_ls = list()
      hinf_ls = list()
      hst_ls = list()
      wvfrt1_ls = list()
      wvfrt2_ls = list()
      
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
        df1[j,3]=dataHst$area_infected[1]/pixelArea
        df1[j,4]=dataIp$area_infected[1]/pixelArea
        df1[j,5]=dataWvfrt1$area_infected[1]/pixelArea
        df1[j,6]=dataWvfrt2$area_infected[1]/pixelArea
        df1[j,7]=dataNo$area_infected[1]/pixelArea
        
        df1[j,8]=dataRd$number_infected[1]
        df1[j,9]=dataHinf$number_infected[1]
        df1[j,10]=dataHst$number_infected[1]
        df1[j,11]=dataIp$number_infected[1]
        df1[j,12]=dataWvfrt1$number_infected[1]
        df1[j,13]=dataWvfrt2$number_infected[1]
        df1[j,14]=dataNo$number_infected[1]
        
        
        no_re=dataNo$infected
        nl = length(no_re)
        no_re=as.matrix(no_re[[nl]])
        no_re = raster(no_re, template = inf)
        no_ls[[j]]= no_re
        
        rd_re=dataRd$infected
        rd_re=as.matrix(rd_re[[nl]])
        rd_re = raster(rd_re, template = inf)
        rd_ls[[j]]= rd_re
        
        hinf_re=dataHinf$infected
        hinf_re=as.matrix(hinf_re[[nl]])
        hinf_re = raster(hinf_re, template = inf)
        hinf_ls[[j]]= hinf_re
        
        hst_re=dataHst$infected
        hst_re=as.matrix(hst_re[[nl]])
        hst_re=raster(hst_re, template = inf)
        hst_ls[[j]]= hst_re
        
        ip_re=dataIp$infected
        ip_re=as.matrix(ip_re[[nl]])
        ip_re=raster(ip_re, template = inf)
        ip_ls[[j]]= ip_re
        
        wvfrt1_re=dataWvfrt1$infected
        wvfrt1_re=as.matrix(wvfrt1_re[[nl]])
        wvfrt1_re=raster(wvfrt1_re, template = inf)
        wvfrt1_ls[[j]]= wvfrt1_re
        
        wvfrt2_re=dataWvfrt2$infected
        wvfrt2_re=as.matrix(wvfrt2_re[[nl]])
        wvfrt2_re=raster(wvfrt2_re, template = inf)
        wvfrt2_ls[[j]]= wvfrt2_re
        
      }
      
      dfsm[i,1:14]=colMeans(df1[1:r,1:14])
      
      print (paste("Year",as.character(i)))
      print( colMeans(df1[1:r,1:14]))
      
      
      infNo_arry = array(unlist(no_ls) , c(dim1,dim2,r))
      infNo = apply(infNo_arry , 1:2 , median )
      inf_No = raster(as.matrix(infNo), template=inf)
      
      
      infRd_arry = array(unlist(rd_ls) , c(dim1,dim2,r))
      infRd = apply(infRd_arry , 1:2 , median )
      inf_Rd = raster(as.matrix(infRd), template=inf)
      
      infHinf_arry = array(unlist(hinf_ls) , c(dim1,dim2,r))
      infHinf = apply(infHinf_arry,1:2 ,median)
      inf_Hinf = raster(as.matrix(infHinf), template=inf)
      
      infHst_arry = array(unlist(hst_ls) , c(dim1,dim2,r))
      infHst = apply(infHst_arry,1:2 ,median)
      inf_Hst = raster(as.matrix(infHst), template=inf)
      
      infIp_arry = array(unlist(ip_ls) , c(dim1,dim2,r))
      infIp = apply(infIp_arry,1:2 ,median)
      inf_Ip = raster(as.matrix(infIp), template=inf)
      
      infWvfrt1_arry = array(unlist(wvfrt1_ls) , c(dim1,dim2,r))
      infWvfrt1 = apply(infWvfrt1_arry,1:2 ,median)
      inf_Wvfrt1 = raster(as.matrix(infWvfrt1), template=inf)
      
      infWvfrt2_arry = array(unlist(wvfrt2_ls) , c(dim1,dim2,r))
      infWvfrt2 = apply(infWvfrt2_arry,1:2 ,median)
      inf_Wvfrt2 = raster(as.matrix(infWvfrt2), template=inf)
      
      
      host_nm = substr(host_file, 1, nchar(host_file)-4)  
      loop_nm = paste("Disp", as.character(p), "Ldis", as.character(ld), "Year", as.character(i),sep="")
      
      no_nm = paste("./Prop03/result/",host_nm, loop_nm, "No.tif",sep="")
      rd_nm = paste("./Prop03/result/",host_nm, loop_nm, "Rd.tif",sep="")
      hinf_nm = paste("./Prop03/result/",host_nm, loop_nm, "Hinf.tif",sep="")
      hst_nm = paste("./Prop03/result/",host_nm, loop_nm, "Hst.tif",sep="")
      ip_nm = paste("./Prop03/result/",host_nm, loop_nm, "Ip.tif",sep="")
      wvfrt1_nm = paste("./Prop03/result/",host_nm, loop_nm, "Wvfrt1.tif",sep="")
      wvfrt2_nm = paste("./Prop03/result/",host_nm, loop_nm, "Wvfrt2.tif",sep="")
      
      writeRaster(inf_No, no_nm)
      writeRaster(inf_Rd, rd_nm)
      writeRaster(inf_Hinf, hinf_nm)
      writeRaster(inf_Hst, hst_nm)
      writeRaster(inf_Ip, ip_nm)
      writeRaster(inf_Wvfrt1, wvfrt1_nm)
      writeRaster(inf_Wvfrt2, wvfrt2_nm)
    }
    
    dfnm = paste("./Prop03/result/","Disp", as.character(p), "Ldis", as.character(ld), sep="")
    dfsm$Year=c(1:4)
    write.csv(dfsm,dfnm)
  }
  
}
print(Sys.time()-t0)



dfsm[1:2,]


# 10:19 ~ 