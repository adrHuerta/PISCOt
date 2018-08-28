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
### source codes 

source('./functions/tools_get_os.R')
source('./functions/interpolation_functions_2.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","normal_obs_dataset.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))
raster_output <- file.path("/media","buntu","D1AB-BCDE","databases","results","monthly_normals","data")
ls()

##################################
#Monthly normal interpolation
##################################

tn_obs_normal <- tn_obs_normal[,-13]
tx_obs_normal <- tx_obs_normal[,-13]

monthly_tn_results <- list() 
monthly_tx_results <- list()

for(j in 1:12){
  
  #tn
  procestn <-  get_variables(MODIS_LST = tn_lst,
                             ALL_STATICS_VAR = rest_cov,
                             OBS_DATA = tn_obs_normal,
                             m = j %>% as.character(),
                             name_var = c("LST","Z","X","Y","DS","TDI"))
  
  monthly_tn_results[[j]] <- regKriging(from_get_variables = procestn)
  
  #tx
  procestx <- get_variables(MODIS_LST = tx_lst, 
                            ALL_STATICS_VAR = rest_cov,
                            OBS_DATA = tx_obs_normal,
                            m = j %>% as.character(),
                            name_var = c("LST","Z","X","Y","DS","TDI"))
  
  monthly_tx_results[[j]] <- regKriging(from_get_variables = procestx)
  
}

merging_tn <- lapply(monthly_tn_results, function(z) z$final_map) %>% brick() 
merging_tx <- lapply(monthly_tx_results, function(z) z$final_map) %>% brick() 

save(monthly_tn_results, monthly_tx_results, 
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_results_R.RData"))

save(merging_tx, merging_tn,
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_rasters.RData"))


####
# cross validation 

library(foreach)
library(doParallel)

cl <- makeCluster(10) #not to overload your computer
registerDoParallel(cl)


foreach(stations = 1:178, .packages=c("foreach","sp")) %dopar% {
  
  leave_one_out_tn <- tn_obs_normal[-stations,]
  tn_val <- foreach(all_cicle = 1:12, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    
    procestx <- get_variables(MODIS_LST = tn_lst, 
                              ALL_STATICS_VAR = rest_cov,
                              OBS_DATA = leave_one_out_tn,
                              m = all_cicle %>% as.character(),
                              name_var = c("LST","Z"))
    
    regKriging(from_get_variables = procestx)$final_map}
  save(tn_val, file = file.path(raster_output,"validation","tn",paste(stations,".RData", sep = "")))
  
  leave_one_out_tx <- tx_obs_normal[-stations,]
  tx_val <- foreach(all_cicle = 1:12, .packages=c("sp","dplyr","automap","gstat")) %dopar% {
    
    
    procestx <- get_variables(MODIS_LST = tx_lst, 
                              ALL_STATICS_VAR = rest_cov,
                              OBS_DATA = leave_one_out_tx,
                              m = all_cicle %>% as.character(),
                              name_var = c("LST","Z"))
    
    regKriging(from_get_variables = procestx)$final_map}
  save(tx_val, file = file.path(raster_output,"validation","tx",paste(stations,".RData", sep = "")))
  
}


stopCluster(cl)



