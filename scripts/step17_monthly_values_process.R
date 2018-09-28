library(MASS)
library(foreach)
library(doParallel)
library(dplyr)
### source codes 

source('./functions/interpolation_functions_2.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","manom_obs_dataset.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","results","monthly_normals","data","monthly_rasters.RData"))
file_path <- "/media/buntu/D1AB-BCDE/databases/results/monthly_values/data"
ls()

###

rest_cov_tn <- crop(rest_cov, extent(merging_tn)) 
rest_cov_tx <- crop(rest_cov, extent(merging_tn)) 

#######
#monthly anomaly interpolation
#######

names(anom_mtn) %>% tail()

cl <- makeCluster(12) #not to overload your computer
registerDoParallel(cl)

foreach(j=1:432, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
  procestn <- get_variables2(CLIM_D = merging_tn,
                             ALL_STATICS_VAR = rest_cov_tn,
                             OBS_DATA = anom_mtn,
                             n = j,
                             name_var = c("CT","TDI"))
  
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
                             name_var = c("CT","TDI"))
  
  anom_tx <- regKriging3(from_get_variables = procestx)
  raster_names_tx <- file.path(file_path, "tx", paste(procestx$date, ".nc", sep = ""))
  writeRaster(anom_tx$final_map,
              raster_names_tx,
              format = "CDF",
              overwrite = F)
}

stopCluster(cl)


#######
#monthly anomaly interpolation # cross validation 
#######

anom_mtn <- anom_mtn[complete.cases(anom_mtn$n_mean) & anom_mtn$n_mean > 25, -c(433,434)]
anom_mtx <- anom_mtx[complete.cases(anom_mtx$n_mean) & anom_mtx$n_mean > 25, -c(433,434)]

cl <- makeCluster(12) #not to overload your computer
registerDoParallel(cl)


foreach(all_cicle = 1:432, .packages=c("foreach","sp")) %dopar% {
  
  tn_val <- foreach(stations = 1:dim(anom_mtn)[1], .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    leave_one_out_tn <- anom_mtn[-stations,]
    
    procestn <- get_variables2(CLIM_D = merging_tn,
                               ALL_STATICS_VAR = rest_cov_tn,
                               OBS_DATA = leave_one_out_tn,
                               n = all_cicle,
                               name_var = c("CT","TDI"))
    
    regKriging3(from_get_variables = procestn)$final_map}
  save(tn_val, file = file.path(file_path,"validation","tn", paste(formatC(all_cicle, width = 3,flag = 0), ".RData", sep = "")))
  
  tx_val <- foreach(stations = 1:dim(anom_mtx)[1], .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    leave_one_out_tx <- anom_mtx[-stations,]
    
    procestx <- get_variables2(CLIM_D = merging_tx,
                               ALL_STATICS_VAR = rest_cov_tx,
                               OBS_DATA = leave_one_out_tx,
                               n = all_cicle,
                               name_var = c("CT","TDI"))
    
      regKriging3(from_get_variables = procestx)$final_map}
  save(tx_val, file = file.path(file_path,"validation","tx", paste(formatC(all_cicle, width = 3,flag = 0), ".RData", sep = "")))
  
}

stopCluster(cl)

