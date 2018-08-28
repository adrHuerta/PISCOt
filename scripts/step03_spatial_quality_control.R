rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(geosphere)
library(norm)

### source codes 

source('./functions/quality_control_temp.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step02_QCDATA_01.RData"))
ls()

############## Spatial Coherence ##########

raw_spat_St_nearest <- get_nearest_stat(data_xy = raw_spat_St[,c("XX", "YY", "CC")])

# 
# raw_dat_St_nearest_TN_4 <-  lapply(raw_spat_St_nearest, function(x) {
#   merge_nearest_stat(data_xy = x, data_ts = raw_temp_TN_4) })

# raw_dat_St_nearest_TX_4 <-  lapply(raw_spat_St_nearest, function(x) {
#   merge_nearest_stat(data_xy = x, data_ts = raw_temp_TX_4) })


# raw_dat_St_nearest_TN_4_NoNA <- lapply(raw_dat_St_nearest_TN_4, function(x){
#   make_data_spqc(x)
# }) 

# raw_dat_St_nearest_TX_4_NoNA <- lapply(raw_dat_St_nearest_TX_4, function(x){
#   make_data_spqc(x)
# }) 

# raw_dat_St_nearest_TN_4_NoNA_filled <- lapply(raw_dat_St_nearest_TN_4_NoNA, function(x){
#   filling_data_spqc(data_base = x)
# })

# raw_dat_St_nearest_TX_4_NoNA_filled <- lapply(raw_dat_St_nearest_TX_4_NoNA, function(x){
#   filling_data_spqc(data_base = x)
# })

raw_dat_St_nearest_TN_4_NoNA_filled <- lapply(raw_spat_St_nearest, function(x) {
  
  stats_n <- x$NN
  data_ts_merge <- raw_temp_TN_4[as.character(stats_n)]
  data_ts_merge <- do.call("cbind", data_ts_merge)
  colnames(data_ts_merge) <- as.character(stats_n)
  return(data_ts_merge)
  
}) %>%  
  lapply(., function(x){ make_data_spqc(x) }) %>% 
  lapply(., function(x){ filling_data_spqc(data_base = x) })

raw_dat_St_nearest_TX_4_NoNA_filled <- lapply(raw_spat_St_nearest, function(x) {
  
  stats_n <- x$NN
  data_ts_merge <- raw_temp_TX_4[as.character(stats_n)]
  data_ts_merge <- do.call("cbind", data_ts_merge)
  colnames(data_ts_merge) <- as.character(stats_n)
  return(data_ts_merge)
  
}) %>%  
  lapply(., function(x){ make_data_spqc(x) }) %>% 
  lapply(., function(x){ filling_data_spqc(data_base = x) })

# rm(raw_spat_St_nearest, raw_dat_St_nearest_TN_4, raw_dat_St_nearest_TX_4)
# rm(raw_dat_St_nearest_TN_4_NoNA, raw_dat_St_nearest_TX_4_NoNA)

raw_temp_TN_5_rest <- lapply(raw_dat_St_nearest_TN_4_NoNA_filled, function(x) spatial_qc(data_base = x))
raw_temp_TX_5_rest <- lapply(raw_dat_St_nearest_TX_4_NoNA_filled, function(x) spatial_qc(data_base = x))
raw_temp_TN_5 <- raw_temp_TN_4
raw_temp_TX_5 <- raw_temp_TX_4

for(j in names(raw_temp_TN_5_rest)){
  
  ts_or <- raw_temp_TN_4[[j]]
  ts_toNA <- raw_temp_TN_5_rest[[j]]
  
  if( is.null(ts_toNA) == TRUE ) {
    
    raw_temp_TN_5[[j]] <- ts_or
    
  } else {
    
    ts_or[time(ts_toNA)] <- NA
    raw_temp_TN_5[[j]] <- ts_or
    
  }
  
  
}


for(j in names(raw_temp_TX_5_rest)){
  
  ts_or <- raw_temp_TX_4[[j]]
  ts_toNA <- raw_temp_TX_5_rest[[j]]
  
  if( is.null(ts_toNA) == TRUE ) {
    
    raw_temp_TX_5[[j]] <- ts_or
    
  } else {
    
    ts_or[time(ts_toNA)] <- NA
    raw_temp_TX_5[[j]] <- ts_or
    
  }
  
  
}
plot(raw_temp_TN_4[["X152"]])
plot(raw_temp_TN_5[["X152"]])


### saving data 

save(raw_temp_TN_5, raw_temp_TX_5, raw_spat_St,
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step03_QCDATA_02.RData"))

save(raw_temp_TN_5_rest,
     raw_temp_TX_5_rest,
     file = file.path("/media","buntu","D1AB-BCDE","databases","results","qc","automatic","step03_QCDATA_02_res.RData"))
