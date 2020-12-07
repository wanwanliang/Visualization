

range_ts = range_m6 (inf, dispersal_rate = natural_distance_scale[[1]], reproductive_rate=reproductive_rate[[1]], Nstep=8, Wcoef)

range_mean = mean(getValues(range_ts),na.rm=T)
range_mean

hst1 = threat_pixel(inf_Hst,width=186,plys_Hst,host=host,budget,buffer,cost_per_meter_sq,25) #30000
hst2 = threat_pixel(inf_Hst,width=2000,plys_Hst,host=host,budget,buffer,cost_per_meter_sq,25) #30000
hst3 = treat_m6(inf,natural_distance_scale[[1]],reproductive_rate[[1]],Nstep=8,Wcoef,ply,host, budget, buffer,cost_per_meter_sq)
#hst4 = threat_pixel(inf_Hst,width=95250,plys_Hst,host=host,budget,buffer,cost_per_meter_sq,25) #30000

# median -- 31625 --995 ; minimum -- 29800 -- 995; maxium -- 46000 -- 991; 2000 -- 981; 1000 -- 984; 3000 --981; 5000 -- 982; 10000 -- 982
#  15650 -- 966;  12800--  ;minimum -- 7750 -- 966; maximum -- 39100 -- 967

ff = c(7950 ,37400, 24650 ,52500)  # minimum
dq = seq(1000, 9000, 2000)
ts = c(ff,dq)
ts


ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,46000,60) 
ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,7750,60) 
ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)

r=10
rate = data.frame(matrix(0,ncol=4))

for (t in 1:7){
  ip_rank_ply = ip_rank(inf_Ip,host, np=8,natural_distance_scale[[1]],reproductive_rate[[1]],Wcoef,ts[t],60) 
  ip2 = ip_treat(ip_rank_ply, budget, buffer, cost_per_meter_sq, inf)
  
  for (j in 1:r){
    random_seed= j*1000+50
    
    
    dataIp <- pops_model(random_seed = random_seed, 
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
                         treatment_maps = ip2,
                         
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
    
    df1[j,t]=dataIp$area_infected[1]/pixelArea
    #df1[j,11]=dataIp$number_infected[1]
    
    #rate[j,1:4] = dataIp$rates[[1]]
    print(j)
  }
  
  print(t)
}


for (j in 1:r){
  random_seed= j*1000+50
  
  
  dataIp <- pops_model(random_seed = random_seed, 
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
                       treatment_maps = ip2,
                       
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
  
  df1[j,4]=dataIp$area_infected[1]/pixelArea
  df1[j,11]=dataIp$number_infected[1]
  
  rate[j,1:4] = dataIp$rates[[1]]
  print(j)
}

# 4, 11
colMeans(df1[1:10,])


treatment_years=c(2019)
start_time <- "2019-03-01"
end_time <- "2019-12-30"
season_month_start = 3
season_month_end = 10

#id1 = (i-1)*8 + 1
#id2 = i*8
#wc = Wcoef[[id1:id2]]

rates_df = data.frame(matrix(0,ncol=4))
r=10

for (j in 1:r){
  random_seed= j*1000+50
  
  dataHst1 <- pops_model(random_seed = random_seed, 
                       use_lethal_temperature = use_lethal_temperature, 
                       lethal_temperature = lethal_temperature, 
                       lethal_temperature_month = lethal_temperature_month,
                       
                       infected = infected_species_No[[1]],
                       susceptible = susceptible_species_No[[1]],
                       total_plants = total_pl[[1]],
                       mortality_on = mortality_on,
                       mortality_tracker = infected_species_No[[1]]*0,
                       mortality = infected_species_No[[1]]*0,
                       treatment_maps = hst1,
                       
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
  
  dataHst2 <- pops_model(random_seed = random_seed, 
                         use_lethal_temperature = use_lethal_temperature, 
                         lethal_temperature = lethal_temperature, 
                         lethal_temperature_month = lethal_temperature_month,
                         
                         infected = infected_species_No[[1]],
                         susceptible = susceptible_species_No[[1]],
                         total_plants = total_pl[[1]],
                         mortality_on = mortality_on,
                         mortality_tracker = infected_species_No[[1]]*0,
                         mortality = infected_species_No[[1]]*0,
                         treatment_maps = hst2,
                         
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
  
  
  dataHst3 <- pops_model(random_seed = random_seed, 
                         use_lethal_temperature = use_lethal_temperature, 
                         lethal_temperature = lethal_temperature, 
                         lethal_temperature_month = lethal_temperature_month,
                         
                         infected = infected_species_No[[1]],
                         susceptible = susceptible_species_No[[1]],
                         total_plants = total_pl[[1]],
                         mortality_on = mortality_on,
                         mortality_tracker = infected_species_No[[1]]*0,
                         mortality = infected_species_No[[1]]*0,
                         treatment_maps = hst3,
                         
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
  

  
  dataIp2 <- pops_model(random_seed = random_seed, 
                         use_lethal_temperature = use_lethal_temperature, 
                         lethal_temperature = lethal_temperature, 
                         lethal_temperature_month = lethal_temperature_month,
                         
                         infected = infected_species_No[[1]],
                         susceptible = susceptible_species_No[[1]],
                         total_plants = total_pl[[1]],
                         mortality_on = mortality_on,
                         mortality_tracker = infected_species_No[[1]]*0,
                         mortality = infected_species_No[[1]]*0,
                         treatment_maps = ip2,
                         
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
  
  
  
  
  df1[j,1]=dataHst1$area_infected[1]/pixelArea
  df1[j,2]=dataHst2$area_infected[1]/pixelArea
  df1[j,3]=dataHst3$area_infected[1]/pixelArea
  df1[j,4]=dataIp2$area_infected[1]/pixelArea
  
  df1[j,8]=dataHst1$number_infected[1]/pixelArea
  df1[j,9]=dataHst2$number_infected[1]/pixelArea
  df1[j,10]=dataHst3$number_infected[1]/pixelArea
  df1[j,11]=dataIp2$number_infected[1]/pixelArea
  
  
  print(j)
}

colMeans(df1[1:r,])



colMeans(df1[1:5,1:14])
