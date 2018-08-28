library(MASS)
library(foreach)
library(doParallel)
library(dplyr)
### source codes 

source('./functions/interpolation_functions_2.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","anom_obs_dataset.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_rasters.RData"))
file_path <- "/media/buntu/D1AB-BCDE/databases/results/monthly_values/data"
ls()

#######
#monthly anomaly interpolation
#######

anom_mtn <- anom_mtn[,-433]
anom_mtx <- anom_mtx[,-433]

rest_cov_tn <- crop(rest_cov, extent(merging_tn)) 
rest_cov_tx <- crop(rest_cov, extent(merging_tn)) 

#anom_tn_results <- list() 

cl <- makeCluster(12) #not to overload your computer
registerDoParallel(cl)

foreach(j=1:432, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
  procestn <- get_variables2(CLIM_D = merging_tn,
                             ALL_STATICS_VAR = rest_cov_tn,
                             OBS_DATA = anom_mtn,
                             n = j,
                             name_var = c("CT"))
  
  anom_tn <- regKriging3(from_get_variables = procestn)
  raster_names_tn <- file.path(file_path, "tn", paste(procestn$date, ".nc", sep = ""))
  writeRaster(anom_tn$final_map,
              raster_names_tn,
              format = "CDF",
              overwrite = F)
  
  ######
  
  procestx <- get_variables2(CLIM_D = merging_tx,
                             ALL_STATICS_VAR = rest_cov_tx,
                             OBS_DATA = anom_mtx,
                             n = j,
                             name_var = c("CT"))
  
  anom_tx <- regKriging3(from_get_variables = procestx)
  raster_names_tx <- file.path(file_path, "tx", paste(procestx$date, ".nc", sep = ""))
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
  tn_val <- foreach(all_cicle = 1:432, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    
    procestn <- get_variables2(CLIM_D = merging_tn,
                               ALL_STATICS_VAR = rest_cov_tn,
                               OBS_DATA = anom_mtn,
                               n = all_cicle,
                               name_var = c("CT"))
    
    regKriging3(from_get_variables = procestx)$final_map}
  save(tn_val, file = file.path(file_path_tn,"validation","tn",paste(stations,".RData", sep = "")))
  
  leave_one_out_tx <- anom_mtx[-stations,]
  tx_val <- foreach(all_cicle = 1:432, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    
    
    procestx <- get_variables2(CLIM_D = merging_tx,
                               ALL_STATICS_VAR = rest_cov_tx,
                               OBS_DATA = anom_mtx,
                               n = all_cicle,
                               name_var = c("CT"))
    
      regKriging3(from_get_variables = procestx)$final_map}
  save(tx_val, file = file.path(file_path_tx,"validation","tx",paste(stations,".RData", sep = "")))
  
}

stopCluster(cl)

