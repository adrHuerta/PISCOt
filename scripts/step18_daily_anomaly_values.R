rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(ggplot2)
library(ggrepel)
library(sp)
library(maptools)
library(raster)
library(gstat)
library(geosphere)

### source codes 

source('./functions/interpolation_functions.R')

###

  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_tn_RASTERS_R.RData"))
  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_tx_RASTERS_R.RData"))
  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_daily_obs_dataset.RData"))
  ls()
  
#### 

  xy_data <- HM_stats_2 %>%
    .[, c("CC", "XX", "YY")]
  coordinates(xy_data) <- ~XX+YY
  projection(xy_data) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  mtn <- extract(merging_tn, xy_data)
  mtx <- extract(merging_tx, xy_data)
  

###################

ts_mclim_HMdata_tx <- apply(mtx, 1, mclim_dts)
anom_dtx <- ((HMdata_dtx_del_2 - ts_mclim_HMdata_tx)) %>% t()
colnames(anom_dtx) <- paste("TX_", gsub("-", "_", colnames(anom_dtx)),  sep = "")
anom_dtx <- cbind(anom_dtx , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(anom_dtx) <- ~XX+YY
projection(anom_dtx) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ts_mclim_HMdata_tn <- apply(mtn, 1, mclim_dts)
anom_dtn <- ((HMdata_dtn_del_2 - ts_mclim_HMdata_tn)) %>%   t()
colnames(anom_dtn) <- paste("TN_", gsub("-", "_", colnames(anom_dtn)),  sep = "")
anom_dtn <- cbind(anom_dtn , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(anom_dtn) <- ~XX+YY
projection(anom_dtn) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

### saving data 

  save(anom_dtn, anom_dtx,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","danom_obs_dataset.RData"))
