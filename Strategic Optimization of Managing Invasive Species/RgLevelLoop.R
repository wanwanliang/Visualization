

library(rgdal)
library(raster)
library(rgeos)
library(sp)
library(PoPS)
library(abind)

setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/Prop10/")

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
buffer = 150
rgs = c(1000, 2000, 5000, 10000)
cost_per_meter_sq=1
cost_per_meter_sq = 1
treatment_years=c(2019)

start_time <- "2019-01-01"
end_time <- "2019-12-30"



no=list(as.matrix(inf*0))
df1=as.data.frame(matrix(0,nrow=10,ncol=8))
dfsm=as.data.frame(matrix(0,nrow=4,ncol=8))
colnames(df1)=c("Ip0","Ip1","Ip2","Ip","Ip0","Ip1","Ip2","Ip")
colnames(dfsm)=c("Ip0","Ip1","Ip2","Ip","Ip0","Ip1","Ip2","Ip")

# 3 sets parameters #

repros = c(2,4,8)
disp = 10
disp = c(5, 10, 20)
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
for (l in 1:3){
  for (b in 1:3){
  
  reproductive_rate[[1]] = repros[b]
  natural_distance_scale[[1]] = disp[b]
  percent_natural_dispersal[[1]] = longdis[l]
  budget = budget
  buffer = buffer
  
  
  

  inf_Ip0=inf
  inf_Ip1=inf
  inf_Ip2=inf
  inf_Ip=inf
  inf_Ip3=inf
 
  
  infected_species_Ip0 = list()
  infected_species_Ip1 = list()
  infected_species_Ip2 = list()
  infected_species_Ip3 = list()
  infected_species_Ip = list()
  
  
  susceptible_species_Ip0 = list()
  susceptible_species_Ip1 = list()
  susceptible_species_Ip2 = list()
  susceptible_species_Ip = list()
  susceptible_species_Ip3 = list()
  
  
  #r=50
  
  
  for (i in 1:3){
    
    d = tempfile()
    dir.create(d)
    setwd(d)
    
    
   
    infected_species_Ip0[[1]] = as.matrix(inf_Ip0)
    infected_species_Ip1[[1]] = as.matrix(inf_Ip1)
    infected_species_Ip[[1]] = as.matrix(inf_Ip)
    infected_species_Ip2[[1]] = as.matrix(inf_Ip2)
    infected_species_Ip3[[1]] = as.matrix(inf_Ip3)
    
    
    
    susceptible_species_Ip0[[1]] <- as.matrix(host - inf_Ip0)
    susceptible_species_Ip1[[1]] <- as.matrix(host - inf_Ip1)
    susceptible_species_Ip[[1]] <- as.matrix(host - inf_Ip)
    susceptible_species_Ip2[[1]] <- as.matrix(host - inf_Ip2)
    susceptible_species_Ip3[[1]] <- as.matrix(host - inf_Ip3)
    
   
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
    
    ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,rg ,20)
    ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)
    print("IPdone")
    
    ip_rank_ply2 = ip_rank(inf_Ip2,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,rgs[4] ,20)
    ip2d = ip_treat(ip_rank_ply2, budget, buffer, cost_per_meter_sq, inf)
    print("IP2done")

    ip_rank_ply2 = ip_rank(inf_Ip2,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,rgs[3] ,20)
    ip2c = ip_treat(ip_rank_ply2, budget, buffer, cost_per_meter_sq, inf)
    print("IP2done")
    
    ip_rank_ply1 = ip_rank(inf_Ip1,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,rgs[2] ,20)
    ip2b = ip_treat(ip_rank_ply1, budget, buffer, cost_per_meter_sq, inf)
    print("IPdone")
    
    ip_rank_ply0 = ip_rank(inf_Ip0,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,rgs[1] ,20)
    ip2a = ip_treat(ip_rank_ply0, budget, buffer, cost_per_meter_sq, inf)
    print("IP2done")
    
    
    ip_ls = stack()
    ip_ls2 = stack()
    ip_ls0 = stack()
    ip_ls1 = stack()
    ip_ls3 = stack()
    
    treatment_years=c(2019)
    start_time <- "2019-03-01"
    end_time <- "2019-12-30"
    season_month_start = 3
    season_month_end = 10
    
    #id1 = (i-1)*8 + 1
    #id2 = i*8
    #wc = Wcoef[[id1:id2]]
    
    print(rg)
    nl=1
    r=100
    
    for (j in 1:r){
      random_seed= j*1000+50
      
      
     
      infected_species_Ip[[1]] = as.matrix(inf_Ip)
      infected_species_Ip0[[1]] = as.matrix(inf_Ip0)
      infected_species_Ip1[[1]] = as.matrix(inf_Ip1)
      infected_species_Ip2[[1]] = as.matrix(inf_Ip2)
      infected_species_Ip3[[1]] = as.matrix(inf_Ip3)
      
      
      
      susceptible_species_Ip[[1]] <- as.matrix(host - inf_Ip)
      susceptible_species_Ip0[[1]] <- as.matrix(host - inf_Ip0)
      susceptible_species_Ip1[[1]] <- as.matrix(host - inf_Ip1)
      susceptible_species_Ip2[[1]] <- as.matrix(host - inf_Ip2)
      susceptible_species_Ip3[[1]] <- as.matrix(host - inf_Ip3)
      
      
      
       
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
                            infected = infected_species_Ip2[[1]],
                            susceptible = susceptible_species_Ip2[[1]],
                            total_plants = total_pl[[1]],
                            mortality_on = mortality_on,
                            mortality_tracker = infected_species_Ip2[[1]]*0,
                            mortality = infected_species_Ip2[[1]]*0,
                            treatment_maps = ip2c,
                            treatment_dates = c("2019-03-01"),
                            pesticide_duration=c(0),
                            resistant = infected_species_Ip2[[1]]*0,
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
    
        dataIp3 <- pops_model(random_seed = random_seed, 
                            use_lethal_temperature = use_lethal_temperature, 
                            lethal_temperature = lethal_temperature, 
                            lethal_temperature_month = lethal_temperature_month,
                            infected = infected_species_Ip3[[1]],
                            susceptible = susceptible_species_Ip3[[1]],
                            total_plants = total_pl[[1]],
                            mortality_on = mortality_on,
                            mortality_tracker = infected_species_Ip3[[1]]*0,
                            mortality = infected_species_Ip3[[1]]*0,
                            treatment_maps = ip2d,
                            treatment_dates = c("2019-03-01"),
                            pesticide_duration=c(0),
                            resistant = infected_species_Ip3[[1]]*0,
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
      
      
      dataIp0 <- pops_model(random_seed = random_seed, 
                            use_lethal_temperature = use_lethal_temperature, 
                            lethal_temperature = lethal_temperature, 
                            lethal_temperature_month = lethal_temperature_month,
                            infected = infected_species_Ip0[[1]],
                            susceptible = susceptible_species_Ip0[[1]],
                            total_plants = total_pl[[1]],
                            mortality_on = mortality_on,
                            mortality_tracker = infected_species_Ip0[[1]]*0,
                            mortality = infected_species_Ip0[[1]]*0,
                            treatment_maps = ip2a,
                            treatment_dates = c("2019-03-01"),
                            pesticide_duration=c(0),
                            resistant = infected_species_Ip0[[1]]*0,
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
      
      
      dataIp1 <- pops_model(random_seed = random_seed, 
                            use_lethal_temperature = use_lethal_temperature, 
                            lethal_temperature = lethal_temperature, 
                            lethal_temperature_month = lethal_temperature_month,
                            infected = infected_species_Ip1[[1]],
                            susceptible = susceptible_species_Ip1[[1]],
                            total_plants = total_pl[[1]],
                            mortality_on = mortality_on,
                            mortality_tracker = infected_species_Ip1[[1]]*0,
                            mortality = infected_species_Ip1[[1]]*0,
                            treatment_maps = ip2b,
                            treatment_dates = c("2019-03-01"),
                            pesticide_duration=c(0),
                            resistant = infected_species_Ip1[[1]]*0,
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
      
      
       
     
      df1[j,1]=dataIp$area_infected[1]/pixelArea
      df1[j,2]=dataIp0$area_infected[1]/pixelArea
      df1[j,3]=dataIp1$area_infected[1]/pixelArea
      df1[j,4]=dataIp2$area_infected[1]/pixelArea
      df1[j,5]=dataIp3$area_infected[1]/pixelArea
      
      
      ip_re=dataIp$infected
      ip_re=as.matrix(ip_re[[nl]])
      ip_re=raster(ip_re, template = inf)
      ip_ls= stack(ip_ls,ip_re)
      
      ip_re3=dataIp3$infected
      ip_re3=as.matrix(ip_re3[[nl]])
      ip_re3=raster(ip_re3, template = inf)
      ip_ls3= stack(ip_ls3,ip_re3)
      
      
      ip_re2=dataIp2$infected
      ip_re2=as.matrix(ip_re2[[nl]])
      ip_re2=raster(ip_re2, template = inf)
      ip_ls2= stack(ip_ls2,ip_re2)
      
      ip_re1=dataIp1$infected
      ip_re1=as.matrix(ip_re1[[nl]])
      ip_re1=raster(ip_re1, template = inf)
      ip_ls1= stack(ip_ls1,ip_re)
      
      ip_re0=dataIp0$infected
      ip_re0=as.matrix(ip_re0[[nl]])
      ip_re0=raster(ip_re0, template = inf)
      ip_ls0= stack(ip_ls0,ip_re0)
     
      
      print(j)
    }
    
    print (paste("Year",as.character(i)))
    
    
    mdv = lapply(1:5, function(x){with(df1, which.min(abs(df1[,x]- median(df1[,x]))))})
    
    
    inf_Ip = raster(ip_ls, layer = mdv[[1]])
    inf_Ip0 = raster(ip_ls0, layer = mdv[[2]])
    inf_Ip1 = raster(ip_ls1, layer = mdv[[3]])
    inf_Ip2 = raster(ip_ls2, layer = mdv[[4]])
    inf_Ip3 = raster(ip_ls3, layer = mdv[[5]])
    
    
    area_ip = df1[mdv[[1]],1]
    area_ip0 = df1[mdv[[1]],2]
    area_ip1 = df1[mdv[[1]],3]
    area_ip2 = df1[mdv[[1]],4]
    area_ip3 = df1[mdv[[1]],5]
    
   
    
    dfsm[i,1:5]=c(area_ip, area_ip0, area_ip1, area_ip2, area_ip3)
    print(dfsm[i,1:5])
    
    
    
    
    host_nm = substr(host_file, 1, nchar(host_file)-4)  
    loop_nm = paste("Ldis",as.character(l), "Rg", as.character(b),"Year", as.character(i),sep="")
    
    ip_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/range/",host_nm, loop_nm, "Ip.tif",sep="")
    ip_nm0 = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/range/",host_nm, loop_nm, "Ip0.tif",sep="")
    ip_nm1 = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/range/",host_nm, loop_nm, "Ip1.tif",sep="")
    ip_nm2 = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/range/",host_nm, loop_nm, "Ip2.tif",sep="")
    ip_nm3 = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/range/",host_nm, loop_nm, "Ip3.tif",sep="")
    
   
    writeRaster(inf_Ip, ip_nm, overwrite=T)
    writeRaster(inf_Ip0, ip_nm0, overwrite=T)
    writeRaster(inf_Ip1, ip_nm1, overwrite=T)
    writeRaster(inf_Ip2, ip_nm2, overwrite=T)
    writeRaster(inf_Ip3, ip_nm3, overwrite=T)
    
    
    file.remove(dir(d, full.names = T))
    
    dfnm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/range/","rg", as.character(b),"Y", as.character(i),".csv" ,sep="")
    #dfsm$Year=c(1:3)
    write.csv(dfsm,dfnm)
    
    print(dfsm[1:3,])
    
  }
  
  dfsm[1:3,]
  
  
  }
}
print(Sys.time()-t0)





# 10:19 ~ 