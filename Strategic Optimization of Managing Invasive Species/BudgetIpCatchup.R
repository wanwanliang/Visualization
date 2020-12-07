

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
budgets=c(1000000,2000000,4000000)
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

repros = 4
disp = 10
longdis = 0.97 

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
for (b in 1:3){
  
  reproductive_rate[[1]] = repros
  natural_distance_scale[[1]] = disp
  percent_natural_dispersal[[1]] = longdis
  budget = budgets[b]
  
  
  
  
  inf_Ip=inf
  
  infected_species_Ip = list()
  
  susceptible_species_Ip = list()
  
  
  r=100
  
  
  for (i in 1:3){
    
  
   
    infected_species_Ip[[1]] = as.matrix(inf_Ip)
    
    susceptible_species_Ip[[1]] <- as.matrix(host - inf_Ip)
    
    
    ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,2000 ,10)
    ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)
    print("IPdone")
    
  
    ip_ls = stack()
    
    
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
      
      
      
      infected_species_Ip[[1]] = as.matrix(inf_Ip)
     
      
      susceptible_species_Ip[[1]] <- as.matrix(host - inf_Ip)

      
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
      
      
      df1[j,6]=dataIp$area_infected[1]/pixelArea
      
      df1[j,16]=dataIp$number_infected[1]
    
      
      
      
      ip_re=dataIp$infected
      nl = length(ip_re)
      ip_re=as.matrix(ip_re[[nl]])
      ip_re=raster(ip_re, template = inf)
      ip_ls= stack(ip_ls,ip_re)
      
     
      print(j)
    }
    
    print (paste("Year",as.character(i)))
    
    
    mdv = lapply(1:10, function(x){with(df1, which.min(abs(df1[,x]- median(df1[,x]))))})
    
    
   
    inf_Ip = raster(ip_ls, layer = mdv[[6]])
    area_ip =  df1[mdv[[6]],6]
   
   
    num_ip = df1[mdv[[6]],15]
  
    
    
    
    
    host_nm = substr(host_file, 1, nchar(host_file)-4)  
    loop_nm = paste("Bg", as.character(b),"Year", as.character(i),sep="")
    
    ip_nm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/result/budget/",host_nm, loop_nm, "Ip2k.tif",sep="")
 
   
    writeRaster(inf_Ip, ip_nm, overwrite=T)
  
    
    
    dfnm = paste("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/Prop03/result/","Budget", as.character(b),"Y", as.character(i),".csv" ,sep="")
    #dfsm$Year=c(1:3)
    write.csv(dfsm,dfnm)
    
  }
  
  
  
}
print(Sys.time()-t0)



dfsm[1:2,]


# 10:19 ~ 