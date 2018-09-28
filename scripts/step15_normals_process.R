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

#rest_cov <- mask(crop(rest_cov, extent(tn_lst)),  tn_lst[[1]])

#################################
#Monthly normal interpolation
#################################

monthly_tn_results <- list() 
monthly_tx_results <- list()

for(j in 1:12){
  
  #tn
  procestn <-  get_variables(MODIS_LST = tn_lst,
                             ALL_STATICS_VAR = rest_cov,
                             OBS_DATA = tn_obs_normal,
                             m = j %>% as.character(),
                             name_var = c("LST","Z","X","Y"))
  
  monthly_tn_results[[j]] <- regGWRKriging(from_get_variables = procestn)
  
  #tx
  procestx <- get_variables(MODIS_LST = tx_lst, 
                            ALL_STATICS_VAR = rest_cov,
                            OBS_DATA = tx_obs_normal,
                            m = j %>% as.character(),
                            name_var = c("LST","Z","X","Y"))
  
  monthly_tx_results[[j]] <- regGWRKriging(from_get_variables = procestx)
  
}

merging_tn <- lapply(monthly_tn_results, function(z) z$final_map) %>% brick() 
merging_tx <- lapply(monthly_tx_results, function(z) z$final_map) %>% brick() 

save(monthly_tn_results, monthly_tx_results, 
     file = file.path(raster_output,"monthly_results_R.RData"))

save(merging_tx, merging_tn,
     file = file.path(raster_output,"monthly_rasters.RData"))


#################################
#Monthly normal interpolation # cross validation
