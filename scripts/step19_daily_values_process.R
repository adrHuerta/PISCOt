rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(automap)
library(sp)
library(maptools)
library(raster)
library(gstat)
library(MASS)
library(foreach)
library(doParallel)
### source codes 

source('/media/buntu/D1AB-BCDE/github_repos/PISCO_Temp/functions/interpolation_functions_2.R')
###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","danom_obs_dataset.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_tx_RASTERS_R.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_tn_RASTERS_R.RData"))
file_path <- "/media/buntu/D1AB-BCDE/databases/results/daily_values/data"
ls()

##################################
#anomaly-daily interpolation
##################################

anom_dtn <- anom_dtn[,-13150]
anom_dtx <- anom_dtx[,-13150]

rest_cov_tn <- crop(rest_cov, extent(merging_tx)) 
rest_cov_tx <- crop(rest_cov, extent(merging_tx)) 

cl <- makeCluster(12) #not to overload your computer
registerDoParallel(cl)

foreach(j=1:13149, .packages=c("sp","dplyr","automap","gstat")) %dopar% {

  procestn <- get_variables2(CLIM_D = merging_tn,
                             ALL_STATICS_VAR = rest_cov_tn,
                             OBS_DATA = anom_dtn,
                             n = j,
                             name_var = c("CT"))
  
  anom_tn <- regKriging3(from_get_variables = procestn, delR = 2.5)
  raster_names_tn <- file.path(file_path, "tn", paste(procestn$date, ".nc", sep = ""))
  writeRaster(anom_tn$final_map,
              raster_names_tn,
              format = "CDF",
              overwrite = F)
  
  ######
  
  procestx <- get_variables2(CLIM_D = merging_tx,
                             ALL_STATICS_VAR = rest_cov_tx,
                             OBS_DATA = anom_dtx,
                             n = j,
                             name_var = c("CT"))
  
  anom_tx <- regKriging3(from_get_variables = procestx, delR = 2.5)
  raster_names_tx <- file.path(file_path, "tn", paste(procestx$date, ".nc", sep = ""))
  writeRaster(anom_tx$final_map,
              raster_names_tx,
              format = "CDF",
              overwrite = F)
  
  }

stopCluster(cl)

####
# cross validation 

cl <- makeCluster(12) #not to overload your computer
registerDoParallel(cl)


foreach(stations = 1:178, .packages=c("foreach","sp")) %dopar% {
  
  leave_one_out_tn <- anom_mtn[-stations,]
  tn_val <- foreach(all_cicle = 1:13149, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    
    procestn <- get_variables2(CLIM_D = merging_tn,
                               ALL_STATICS_VAR = rest_cov_tn,
                               OBS_DATA = anom_mtn,
                               n = all_cicle,
                               name_var = c("CT"))
    
    regKriging3(from_get_variables = procestx)$final_map}
  save(tn_val, file = file.path(file_path_tn,"validation","tn",paste(stations,".RData", sep = "")))
  
  leave_one_out_tx <- anom_mtx[-stations,]
  tx_val <- foreach(all_cicle = 1:13149, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    
    
    procestx <- get_variables2(CLIM_D = merging_tx,
                               ALL_STATICS_VAR = rest_cov_tx,
                               OBS_DATA = anom_mtx,
                               n = all_cicle,
                               name_var = c("CT"))
    
    regKriging3(from_get_variables = procestx)$final_map}
  save(tx_val, file = file.path(file_path_tx,"validation","tx",paste(stations,".RData", sep = "")))
  
}

stopCluster(cl)

