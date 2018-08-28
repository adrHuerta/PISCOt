rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(maptools)
library(rgdal)
library(geosphere)

### source codes 

source('./functions/imputation_functions.R')
source('./functions/quality_control_temp.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step09_monthlyDATA_06.RData"))

#########
## monthly 
########

tx_const <- Mdata_txx %>% 
  as.list() %>%
  lapply(function(z) IQR_REA_seasonal(z, umb = 2) ) 

tn_const <- Mdata_tnn %>% 
  as.list() %>%
  lapply(function(z) IQR_REA_seasonal(z, umb = 2) ) 

tx_const_filled <- lapply(tx_const, function(z) z$Filled)
tx_const_filled <- do.call(cbind, tx_const_filled)
colnames(tx_const_filled) <- colnames(Mdata_txx)

tn_const_filled <- lapply(tn_const, function(z) z$Filled)
tn_const_filled <- do.call(cbind, tn_const_filled)
colnames(tn_const_filled) <- colnames(Mdata_tnn)

tx_const_filled_del <- lapply(tx_const, function(z) z$delete_values)
names(tx_const_filled_del) <- colnames(Mdata_txx)
tn_const_filled_del <- lapply(tn_const, function(z) z$delete_values)
names(tn_const_filled_del) <- colnames(Mdata_tnn)

#######

dtr_no_good <- (tx_const_filled - tn_const_filled) %>%
  as.list() %>%
  lapply(., function(z) z[z==0 | z < 0]) %>%
  .[lapply(., function(z) dim(z)[1]) != 0]

dtr_no_good_tx <- lapply(dtr_no_good, function(z){
  climatological_monthly_values <- monthly_clim_values(tx_const_filled[,names(z)]) 
  dates_to_fill <- match(format(time(z), "%m-%d"),  climatological_monthly_values[,1])
  xts(climatological_monthly_values[dates_to_fill,2], time(z))
  })

dtr_no_good_tn <- lapply(dtr_no_good, function(z){
  climatological_monthly_values <- monthly_clim_values(tn_const_filled[,names(z)]) 
  dates_to_fill <- match(format(time(z), "%m-%d"),  climatological_monthly_values[,1])
  xts(climatological_monthly_values[dates_to_fill,2], time(z))
})


for(i in names(dtr_no_good)){
  tx_const_filled[, i][time(dtr_no_good_tx[[i]])] <- coredata(dtr_no_good_tx[[i]]) 
  tn_const_filled[, i][time(dtr_no_good_tn[[i]])] <- coredata(dtr_no_good_tn[[i]]) 
  }
  

dtr_no_good <- (tx_const_filled - tn_const_filled) %>%
  as.list() %>%
  lapply(., function(z) z[z==0 | z < 0]) %>%
  .[lapply(., function(z) dim(z)[1]) != 0]

#####
#last view()
# 
# for(xs in 1:dim(tx_const_filled)[2]){
#   jpeg(paste("/media/buntu/D1AB-BCDE/databases/results/qc/infilling/all/",xs, "_",".jpg", sep = ""), width = 900, height = 750)
#   plot(cbind(tx_const_filled[,xs], tn_const_filled[,xs]) %>% zoo(), type = "p", cex = 0.5)
#   #Sys.sleep(2)
#   dev.off()
# }


Mdata_tx <- tx_const_filled
Mdata_tn <- tn_const_filled


#########
## daily 
########

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step09_dailyDATA_06.RData"))

tx_const <- Ddata_txx %>% 
  as.list() %>%
  lapply(function(z) IQR_REA_ev(z, umb = 4) ) 

tn_const <- Dddata_tnn %>% 
  as.list() %>%
  lapply(function(z) IQR_REA_ev(z, umb = 4) ) 

tx_const_filled <- lapply(tx_const, function(z) z$Filled)
tx_const_filled <- do.call(cbind, tx_const_filled)
colnames(tx_const_filled) <- colnames(Ddata_txx)

tn_const_filled <- lapply(tn_const, function(z) z$Filled)
tn_const_filled <- do.call(cbind, tn_const_filled)
colnames(tn_const_filled) <- colnames(Dddata_tnn)

tx_const_filled_del <- lapply(tx_const, function(z) z$delete_values)
names(tx_const_filled_del) <- colnames(Ddata_txx)
tn_const_filled_del <- lapply(tn_const, function(z) z$delete_values)
names(tn_const_filled_del) <- colnames(Dddata_tnn)

#######

dtr_no_good <- (tx_const_filled - tn_const_filled) %>%
  as.list() %>%
  lapply(., function(z) z[z==0 | z < 0]) %>%
  .[lapply(., function(z) dim(z)[1]) != 0]

dtr_no_good_tx <- lapply(dtr_no_good, function(z){
  climatological_daily_values <- daily_clim_values(tx_const_filled[,names(z)]) 
  dates_to_fill <- match(format(time(z), "%m-%d"),  climatological_daily_values[,1])
  xts(climatological_daily_values[dates_to_fill,2], time(z))
})

dtr_no_good_tn <- lapply(dtr_no_good, function(z){
  climatological_daily_values <- daily_clim_values(tn_const_filled[,names(z)]) 
  dates_to_fill <- match(format(time(z), "%m-%d"),  climatological_daily_values[,1])
  xts(climatological_daily_values[dates_to_fill,2], time(z))
})


for(i in names(dtr_no_good)){
  tx_const_filled[, i][time(dtr_no_good_tx[[i]])] <- coredata(dtr_no_good_tx[[i]]) 
  tn_const_filled[, i][time(dtr_no_good_tn[[i]])] <- coredata(dtr_no_good_tn[[i]]) 
}


dtr_no_good <- (tx_const_filled - tn_const_filled) %>%
  as.list() %>%
  lapply(., function(z) z[z==0 | z < 0]) %>%
  .[lapply(., function(z) dim(z)[1]) != 0]

#####
#last view()
# 
# for(xs in 1:dim(tx_const_filled)[2]){
#   jpeg(paste("/media/buntu/D1AB-BCDE/databases/results/qc/infilling/all/",xs, "_",".jpg", sep = ""), width = 900, height = 750)
#   plot(cbind(tx_const_filled[,xs], tn_const_filled[,xs]) %>% zoo(), type = "p", cex = 0.5)
#   #Sys.sleep(2)
#   dev.off()
# }


Ddata_tx <- tx_const_filled
Ddata_tn <- tn_const_filled


isTRUE(identical(Ddstats_s,Mdstats_s))
stats_s <- Mdstats_s
          
###################

save(Ddata_tx, Ddata_tn, Mdata_tx, Mdata_tn, stats_s,
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step10_FDATA_06.RData"))

