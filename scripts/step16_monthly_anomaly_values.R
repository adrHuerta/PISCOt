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

ts_mclim_HMdata_tx <- apply(mtx, 1, function(z) mclim_dts(z, ts_t = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month")))
anom_mtx <- ((HMdata_mtx_del_2 - ts_mclim_HMdata_tx)) %>% t()
colnames(anom_mtx) <- paste("TX_", gsub("-", "_", colnames(anom_mtx)),  sep = "")
anom_mtx <- cbind(anom_mtx , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(anom_mtx) <- ~XX+YY
projection(anom_mtx) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

mtx <- HMdata_mtx_del_2 %>% t()
colnames(mtx) <- paste("TX_", gsub("-", "_", colnames(mtx)),  sep = "")
mtx <- cbind(mtx , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(mtx) <- ~XX+YY
projection(mtx) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ts_mclim_HMdata_tn <- apply(mtn, 1, function(z) mclim_dts(z, ts_t = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month")))
anom_mtn <- ((HMdata_mtn_del_2 - ts_mclim_HMdata_tn)) %>%   t()
colnames(anom_mtn) <- paste("TN_", gsub("-", "_", colnames(anom_mtn)),  sep = "")
anom_mtn <- cbind(anom_mtn , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(anom_mtn) <- ~XX+YY
projection(anom_mtn) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

mtn <- HMdata_mtn_del_2  %>%   t()
colnames(mtn) <- paste("TN_", gsub("-", "_", colnames(mtn)),  sep = "")
mtn <- cbind(mtn , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(mtn) <- ~XX+YY
projection(mtn) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

### saving data 

  save(anom_mtn, anom_mtx, mtn, mtx,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","manom_obs_dataset.RData"))
