rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(geosphere)
library(norm)

### source codes 

source('./functions/quality_control_temp.R')

###

  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step01_RAWDATA.RData"))
  ls()

############## general qc #################

raw_temp_TN_0 <- lapply(raw_temp_TN, function(x){ 
  duplicate_dates(data_base = x)$data_base
  })

raw_temp_TN_0_rest <- lapply(raw_temp_TN, function(x){ 
  duplicate_dates(data_base = x)$duple_vals
})

raw_temp_TX_0 <- lapply(raw_temp_TX, function(x){ 
  duplicate_dates(data_base = x, col_names = c("TT", "TX"))$data_base
})

raw_temp_TX_0_rest <- lapply(raw_temp_TX, function(x){ 
  duplicate_dates(data_base = x,  col_names = c("TT", "TX"))$duple_vals
})

#looking bad dates format

raw_temp_TN_0_1 <- lapply(raw_temp_TN_0, function(x){ 
  looking_wrong_dates(data_base = x)$data_base
})

raw_temp_TN_0_1_rest <- lapply(raw_temp_TN_0, function(x){ 
  looking_wrong_dates(data_base = x)$duple_vals
})

raw_temp_TX_0_1 <- lapply(raw_temp_TX_0, function(x){ 
  looking_wrong_dates(data_base = x, col_names = c("TT", "TX"))$data_base
})

raw_temp_TX_0_1_rest <- lapply(raw_temp_TX_0, function(x){ 
  looking_wrong_dates(data_base = x,  col_names = c("TT", "TX"))$duple_vals
})

############## Extreme value check #################

raw_temp_TN_1 <- lapply(raw_temp_TN_0_1, function(x){ 
  fixed_ranges(data_base = x)$data_base
})

raw_temp_TN_1_rest <- lapply(raw_temp_TN_0_1, function(x){ 
  fixed_ranges(data_base = x)$data_del
})

raw_temp_TX_1 <- lapply(raw_temp_TX_0_1, function(x){ 
  fixed_ranges(data_base = x, 
               col_names = c("TT", "TX"), 
               umb = c(min_val = -10, max_val = 60))$data_base
})

raw_temp_TX_1_rest <- lapply(raw_temp_TX_0_1, function(x){ 
  fixed_ranges(data_base = x, 
               col_names = c("TT", "TX"),
               umb = c(min_val = -10, max_val = 60))$data_del
})


############## Consisteny among variables 

raw_temp_TN_2 <- list()
raw_temp_TX_2 <- list()
raw_temp_TNTX_2_rest <- list()

for(i in 1:length(raw_temp_TN_1)){
  
  resTN <- raw_temp_TN_1[[i]]
  resTX <- raw_temp_TX_1[[i]]
  
  res_funct <- consisteny_amg_var(data_base_tn = resTN,
                                  data_base_tx = resTX)
  
  raw_temp_TN_2[[i]] <- res_funct$data_base$data_res_TN
  raw_temp_TX_2[[i]] <- res_funct$data_base$data_res_TX
  raw_temp_TNTX_2_rest[[i]] <- res_funct$data_del
  
  }

names(raw_temp_TN_2) <- names(raw_temp_TN_1)
names(raw_temp_TX_2) <- names(raw_temp_TN_1)
names(raw_temp_TNTX_2_rest) <- names(raw_temp_TN_1)
# for(i in 1:length(raw_temp_TN_1)){
#   
#   print(consisteny_amg_var(data_base_tn = raw_temp_TN_1[[i]],
#                            data_base_tx = raw_temp_TX_1[[i]])$data_del)
#   
# }

##### 
#limits: Q25 - 4*IRQ < Temp | Q75 + IRQ*4 > Temp

raw_temp_TN_2_5 <- lapply(raw_temp_TN_2, function(x){ 
  IQR_ev(data_base = x)$data_base
})

raw_temp_TN_2_5_rest <- lapply(raw_temp_TN_2, function(x){ 
  IQR_ev(data_base = x)$data_del
})

raw_temp_TX_2_5 <- lapply(raw_temp_TX_2, function(x){ 
  IQR_ev(data_base = x)$data_base
})

raw_temp_TX_2_5_rest <- lapply(raw_temp_TX_2, function(x){ 
  IQR_ev(data_base = x)$data_del
})


############## Filling dates ##################3


raw_temp_TN_2_5 <- lapply(raw_temp_TN_2_5, function(x){
  filling_dates(data_base = x)
})

raw_temp_TX_2_5 <- lapply(raw_temp_TX_2_5, function(x){
  filling_dates(data_base = x, col_names = c("TT", "TX"))
})


############## Temporal Coherence ##########

#CCC

raw_temp_TN_3 <- lapply(raw_temp_TN_2_5, function(x){
  tempC_ccc(x)$data_base
})

raw_temp_TN_3_rest <- lapply(raw_temp_TN_2_5, function(x){
  tempC_ccc(x)$data_del
})

raw_temp_TX_3 <- lapply(raw_temp_TX_2_5, function(x){
  tempC_ccc(x)$data_base
})

raw_temp_TX_3_rest <- lapply(raw_temp_TX_2_5, function(x){
  tempC_ccc(x)$data_del
})
# 
# raw_temp_TN_3[["156401"]][raw_temp_TN_3_rest[["156401"]]$tt]
# raw_temp_TX_3[["155107"]][raw_temp_TX_3_rest[["155107"]]$tt]
# 

#Jumps

raw_temp_TN_4 <- lapply(raw_temp_TN_3, function(x){
  tempC_jumps(data_base = x)$data_base
})

raw_temp_TN_4_rest <- lapply(raw_temp_TN_3, function(x){
  tempC_jumps(data_base = x)$data_del
})


raw_temp_TX_4 <- lapply(raw_temp_TX_3, function(x){
  tempC_jumps(data_base = x)$data_base
})

raw_temp_TX_4_rest <- lapply(raw_temp_TX_3, function(x){
  tempC_jumps(data_base = x)$data_del
})

####

# raw_temp_TX_4 <- do.call("cbind", raw_temp_TX_4)
# raw_temp_TN_4 <- do.call("cbind", raw_temp_TN_4)

#### 
  
  save(raw_temp_TN_4, raw_temp_TX_4, raw_spat_St,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step02_QCDATA_01.RData"))
  
  save(raw_temp_TN_0_rest,
       raw_temp_TX_0_rest,
       raw_temp_TN_0_1_rest,
       raw_temp_TX_0_1_rest,
       raw_temp_TN_1_rest,
       raw_temp_TX_1_rest,
       raw_temp_TNTX_2_rest,
       raw_temp_TN_2_5_rest,
       raw_temp_TX_2_5_rest,
       raw_temp_TN_3_rest,
       raw_temp_TX_3_rest,
       raw_temp_TN_4_rest,
       raw_temp_TX_4_rest,
       file = file.path("/media","buntu","D1AB-BCDE","databases","results","qc","automatic","step02_QCDATA_01_res.RData"))
