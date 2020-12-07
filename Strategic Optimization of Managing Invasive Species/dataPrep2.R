
# Setup data
library(PoPS)
library(raster)
library(doParallel)
library(iterators)
library(foreach)
library(parallel)
library(rgdal)

setwd("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3")

infected_file <- "Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/Inf2.tif"
total_plants_file <- "Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/HostTotal.tif"
temperature_file <- ""
temperature_coefficient_file <- "Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/WeatherCoefficient.tif"
host_file <- "RC10.tif"
host_file2 = "Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/Prop10/RC10.tif"
Wcoef = raster("Q:/Shared drives/APHIS  Projects/eRADS/workingPapers/simulation/AllFiles3/Wcoef.tif")

inf=raster(infected_file)
inf[is.na(inf)]=0
#inf=reclassify(inf,c(NA,0))
host=raster(host_file2)
host[is.na(host)]=0
host = host + inf
host[host>100]=100
#host=reclassify(host,c(NA,0))
total_plants=raster(total_plants_file)
#total_plants=reclassify(total_plants,c(NA,0))
weather_coefficient=stack(temperature_coefficient_file)
#weather_coefficient=reclassify(weather_coefficient,c(NA,0))


total_plants <- as.matrix(total_plants)
total_pl = list()
total_pl[[1]] = total_plants

#wc=list()
#temperature=list()

#for (i in 1:72){
#  wc[[i]] = as.matrix(raster(temperature_coefficient_file,band=i))
#  temperature[[i]]= as.matrix(inf*0)
#  print(i)
#}


#save(wc,file="wc.RData")
#save(temperature,file="temperature.RData")

# sub =subset(weather_coefficient, 1:48)
# Wcoef = mean(sub)
 
#save(Wcoef, file="Wcoef.RData")

load("wc.RData")
load("temperature.RData")
#load("Wcoef.RData")

precipitation_coefficient_file <-""
use_lethal_temperature <- FALSE
temp <- TRUE
precip <- FALSE
season_month_start <- 3
season_month_end <- 10
time_step <- "month"

lethal_temperature <- -35
lethal_temperature_month <- as.integer(1)
random_seed <- 42
treatments_file <- ""
treatment_years <- c(2019)
treatment_month <- as.integer(3)
treatment_method <- "all infected"
management <- TRUE
mortality_on <- FALSE
mortality_rate <- 0
mortality_time_lag <- 0
reproductive_rate <- list() 
percent_natural_dispersal <- list()
#natural_kernel_type <- c("exponential", "exponential")
natural_kernel_type <- c("cauchy", "cauchy")

anthropogenic_kernel_type <- c("cauchy", "cauchy")
natural_distance_scale <- list()
anthropogenic_distance_scale <- list()
natural_dir <- c("NONE", "NONE")
natural_kappa <- c(0, 0)
anthropogenic_dir <- c("NONE", "NONE")
anthropogenic_kappa <- c(0, 0)

# typical data prep


if (time_step == "week") {
  number_of_time_steps <- (end_time-start_time+1)*52
} else if (time_step == "month") {
  number_of_time_steps <- (end_time-start_time+1)*12
} else if (time_step == "day") {
  number_of_time_steps <- (end_time-start_time+1)*365
}

number_of_years <- end_time-start_time + 1


if(percent_natural_dispersal == 1.0) {
  use_anthropogenic_kernel = FALSE
} else if (percent_natural_dispersal < 1.0  && percent_natural_dispersal >= 0.0) {
  use_anthropogenic_kernel = TRUE
} else {
  return("Percent natural dispersal must be between 0.0 and 1.0")
}

infected = inf
ew_res <- xres(infected)
ns_res <- yres(infected)
num_cols <- raster::ncol(infected)
num_rows <- raster::nrow(infected)

mortality_tracker <- infected
mortality_tracker[] <- 0

total_plants <- as.matrix(total_plants)
mortality_tracker <- as.matrix(mortality_tracker)
mortality <- mortality_tracker


# only for first year
Tcoef=Wcoef
weather=T
ew_res=xres(inf)
ns_res=yres(inf)
num_rows=nrow(inf)
num_cols=ncol(inf)
time_step="month"

