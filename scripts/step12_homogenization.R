rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(ggplot2)
library(ggrepel)
library(sp)
library(maptools)
library(rgdal)
library(geosphere)
library(snht)
library(akima)

### source codes 

source('./functions/quality_control_temp.R')
source('./functions/hmg_functions.R')
source('./functions/imputation_functions.R')
###

  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step10_FDATA_06.RData"))
  load("./data/shp_dataset.RData")
  ls()

#######

data_hm <- get_nearest_stat(data_xy = stats_s[, c("XX", "YY", "CC", "GGR")],
                            nearest = T)

#######################
####### TX
#######################

qcstat_HM <- stats_s %>% 
  .[, c("XX","YY","CC","GGR")] %>%
  get_nearest_stat(nearest = T) %>%
  lapply(., function(x){ 
    r_r <- x[1,4]
    colnames(x) <- c("XX","YY","NN","GGR","D")
    res_R <- subset(x, GGR == r_r) %>% .[1:5, ] %>% .[complete.cases(.),]
    res_noR <- subset(x, GGR != r_r)
    
    if( dim(res_R)[1] < 5 ){
      res_R <- rbind(res_R, res_noR)[1:5, ]
    }
    
    return(res_R)
    #x[1:4, ] 
     }) %>%
  lapply(., function(x) get_dist_matrix(data_XY = x[, c("XX", "YY", "NN")]))

####
# first iteration
####

daily_TXHOMGE <- list()
monthly_TXHOMGE <- list()
TXbreaks_monthly_values <- list()
#z = 174
for(z in 1:length(qcstat_HM))
{
  
  distSize  <- qcstat_HM[[z]]
  dailytTEMP <- Ddata_tx[, colnames(distSize)[1] ]
  monthlyTEMP <- Mdata_tx[, colnames(distSize)[1]]
  monthlyDB <- Mdata_tx[, colnames(distSize) ]
 # SEASONAL_data <- monthlyDB %>% as.list() %>% lapply(seasonal_timeseries_r) 
  SEASONAL_data <- monthlyDB %>% as.list() %>% lapply(seasonal_timeseries_rb) 
  
  # SEASONAL_data_order <- list(x12 = lapply(SEASONAL_data, function(x) x$Dec) %>% do.call(cbind, .), 
  #                             x1 = lapply(SEASONAL_data, function(x) x$Jan) %>% do.call(cbind, .),
  #                             x2 = lapply(SEASONAL_data, function(x) x$Feb) %>% do.call(cbind, .),
  #                             x3 = lapply(SEASONAL_data, function(x) x$Mar) %>% do.call(cbind, .), 
  #                             x4 = lapply(SEASONAL_data, function(x) x$Abr) %>% do.call(cbind, .),
  #                             x5 = lapply(SEASONAL_data, function(x) x$May) %>% do.call(cbind, .),
  #                             x6 = lapply(SEASONAL_data, function(x) x$Jun) %>% do.call(cbind, .),
  #                             x7 = lapply(SEASONAL_data, function(x) x$Jul) %>% do.call(cbind, .),
  #                             x8 = lapply(SEASONAL_data, function(x) x$Aug) %>% do.call(cbind, .),
  #                             x9 = lapply(SEASONAL_data, function(x) x$Sep) %>% do.call(cbind, .),
  #                             x10 = lapply(SEASONAL_data, function(x) x$Oct) %>% do.call(cbind, .),
  #                             x11 = lapply(SEASONAL_data, function(x) x$Nov) %>% do.call(cbind, .)) 
  
  SEASONAL_data_order <-  list(DJF = lapply(SEASONAL_data, function(x) x$DJF) %>% do.call(cbind, .), 
                               MAM = lapply(SEASONAL_data, function(x) x$MAM) %>% do.call(cbind, .),
                               JJA = lapply(SEASONAL_data, function(x) x$JJA) %>% do.call(cbind, .),
                               SON = lapply(SEASONAL_data, function(x) x$SON) %>% do.call(cbind, .)) 
  
  
  # resM_monhtly <- seasonal_pha(database_list = SEASONAL_data_order,
  #                     dat_mat_XY = distSize, 
  #                     monthlyTS = monthlyTEMP)
  resM <- seasonal_phab(database_list = SEASONAL_data_order,
                        dat_mat_XY = distSize, 
                        monthlyTS = monthlyTEMP)
  # plot(monthlyTEMP)
  # plot(resM_monhtly$TS_Corrected)
  # plot(resM_seasonal$TS_Corrected)
  
  # plot(cbind(monthlyTEMP, resM_monhtly$TS_Corrected, resM_seasonal$TS_Corrected) %>% zoo, 
  #      plot.type = "single", lwd = c(1,2,3), col = c(1,2,3))
  
 # plot(resM$monthlyFacts)
  resD <- TOdailyHomg(dailyTS = dailytTEMP,
                      monthlyFacts = resM$monthlyFacts )

  # plot(cbind(resM$TS_Corrected, monthlyTEMP) %>% zoo, main =  colnames(distSize)[1], col = c(1,2), lwd = 3, plot.type = "single")
  
  daily_TXHOMGE[[z]] <- resD$TS_Corrected
  monthly_TXHOMGE[[z]] <- resM$TS_Corrected
  TXbreaks_monthly_values[[z]] <- monthlyTEMP - resM$TS_Corrected  
}

daily_TXHOMGE <- do.call("cbind", daily_TXHOMGE)
monthly_TXHOMGE <- do.call("cbind", monthly_TXHOMGE)
colnames(daily_TXHOMGE) <- names(qcstat_HM)
colnames(monthly_TXHOMGE) <- names(qcstat_HM)

####
# second iteration
####

daily_TXHOMGE2 <- list()
monthly_TXHOMGE2 <- list()
TXbreaks_monthly_values2 <- list()

for(z in 1:length(qcstat_HM))
{
  
  distSize  <- qcstat_HM[[z]]
  dailytTEMP <- daily_TXHOMGE[, colnames(distSize)[1] ]
  monthlyTEMP <- monthly_TXHOMGE[, colnames(distSize)[1]]
  monthlyDB <- monthly_TXHOMGE[, colnames(distSize) ]
  SEASONAL_data <- monthlyDB %>% as.list() %>% lapply(seasonal_timeseries_rb) 
  
  SEASONAL_data_order <-  list(DJF = lapply(SEASONAL_data, function(x) x$DJF) %>% do.call(cbind, .), 
                               MAM = lapply(SEASONAL_data, function(x) x$MAM) %>% do.call(cbind, .),
                               JJA = lapply(SEASONAL_data, function(x) x$JJA) %>% do.call(cbind, .),
                               SON = lapply(SEASONAL_data, function(x) x$SON) %>% do.call(cbind, .)) 
  
  
  resM <- seasonal_phab(database_list = SEASONAL_data_order,
                       dat_mat_XY = distSize, 
                       monthlyTS = monthlyTEMP)
  
  resD <- TOdailyHomg(dailyTS = dailytTEMP,
                      monthlyFacts = resM$monthlyFacts )
  
  # plot(cbind(resM$TS_Corrected, monthlyTEMP) %>% zoo, main =  colnames(distSize)[1], col = c(1,2), lwd = 3, plot.type = "single")
  
  daily_TXHOMGE2[[z]] <- resD$TS_Corrected
  monthly_TXHOMGE2[[z]] <- resM$TS_Corrected
  TXbreaks_monthly_values2[[z]] <- monthlyTEMP - resM$TS_Corrected 
}

daily_TXHOMGE2 <- do.call("cbind", daily_TXHOMGE2)
monthly_TXHOMGE2 <- do.call("cbind", monthly_TXHOMGE2)
colnames(daily_TXHOMGE2) <- names(qcstat_HM)
colnames(monthly_TXHOMGE2) <- names(qcstat_HM)

#######################
####### TN
#######################

qcstat_HM <- stats_s %>% 
  .[, c("XX","YY","CC","GGR")] %>%
  get_nearest_stat(nearest = T) %>%
  lapply(., function(x){ 
    r_r <- x[1,4]
    colnames(x) <- c("XX","YY","NN","GGR","D")
    res_R <- subset(x, GGR == r_r) %>% .[1:5, ] %>% .[complete.cases(.),]
    res_noR <- subset(x, GGR != r_r)
    
    if( dim(res_R)[1] < 5 ){
      res_R <- rbind(res_R, res_noR)[1:5, ]
    }
    
    return(res_R)
    #x[1:4, ] 
  }) %>%
  lapply(., function(x) get_dist_matrix(data_XY = x[, c("XX", "YY", "NN")]))


####
# first iteration
####

daily_TNHOMGE <- list()
monthly_TNHOMGE <- list()
TNbreaks_monthly_values <- list()

for(z in 1:length(qcstat_HM))
{
  
  distSize  <- qcstat_HM[[z]]
  dailytTEMP <- Ddata_tn[, colnames(distSize)[1] ]
  monthlyTEMP <- Mdata_tn[, colnames(distSize)[1] ]
  monthlyDB <- Mdata_tn[, colnames(distSize)]
  
  SEASONAL_data <- monthlyDB %>% as.list() %>% lapply(seasonal_timeseries_rb) 
  
  SEASONAL_data_order <-  list(DJF = lapply(SEASONAL_data, function(x) x$DJF) %>% do.call(cbind, .), 
                               MAM = lapply(SEASONAL_data, function(x) x$MAM) %>% do.call(cbind, .),
                               JJA = lapply(SEASONAL_data, function(x) x$JJA) %>% do.call(cbind, .),
                               SON = lapply(SEASONAL_data, function(x) x$SON) %>% do.call(cbind, .)) 
  
  
  resM <- seasonal_phab(database_list = SEASONAL_data_order,
                       dat_mat_XY = distSize, 
                       monthlyTS = monthlyTEMP)
  
  resD <- TOdailyHomg(dailyTS = dailytTEMP,
                      monthlyFacts = resM$monthlyFacts )
  
  # plot(cbind(resM$TS_Corrected, monthlyTEMP, resM$monthlyFacts) %>% zoo, main =  colnames(distSize)[1], col = c(1,2), lwd = 3, plot.type = "multiple")
  
  daily_TNHOMGE[[z]] <- resD$TS_Corrected
  monthly_TNHOMGE[[z]] <- resM$TS_Corrected
  TNbreaks_monthly_values[[z]] <- monthlyTEMP - resM$TS_Corrected
}

daily_TNHOMGE <- do.call("cbind", daily_TNHOMGE)
monthly_TNHOMGE <- do.call("cbind", monthly_TNHOMGE)
colnames(daily_TNHOMGE) <- names(qcstat_HM)
colnames(monthly_TNHOMGE) <- names(qcstat_HM)

####
# second iteration
####

daily_TNHOMGE2 <- list()
monthly_TNHOMGE2 <- list()
TNbreaks_monthly_values2 <- list()

for(z in 1:length(qcstat_HM))
{
 
  distSize  <- qcstat_HM[[z]]
  dailytTEMP <- daily_TNHOMGE[, colnames(distSize)[1] ]
  monthlyTEMP <- monthly_TNHOMGE[, colnames(distSize)[1] ]
  monthlyDB <- monthly_TNHOMGE[, colnames(distSize)]

  SEASONAL_data <- monthlyDB %>% as.list() %>% lapply(seasonal_timeseries_rb) 
  
  SEASONAL_data_order <-  list(DJF = lapply(SEASONAL_data, function(x) x$DJF) %>% do.call(cbind, .), 
                               MAM = lapply(SEASONAL_data, function(x) x$MAM) %>% do.call(cbind, .),
                               JJA = lapply(SEASONAL_data, function(x) x$JJA) %>% do.call(cbind, .),
                               SON = lapply(SEASONAL_data, function(x) x$SON) %>% do.call(cbind, .)) 
  
  
  
  resM <- seasonal_phab(database_list = SEASONAL_data_order,
                       dat_mat_XY = distSize, 
                       monthlyTS = monthlyTEMP)
  
  resD <- TOdailyHomg(dailyTS = dailytTEMP,
                      monthlyFacts = resM$monthlyFacts )
  
  # plot(cbind(resM$TS_Corrected, monthlyTEMP, resM$monthlyFacts) %>% zoo, main =  colnames(distSize)[1], col = c(1,2), lwd = 3, plot.type = "multiple")
  
  daily_TNHOMGE2[[z]] <- resD$TS_Corrected
  monthly_TNHOMGE2[[z]] <- resM$TS_Corrected
  TNbreaks_monthly_values2[[z]] <- monthlyTEMP - resM$TS_Corrected
}

daily_TNHOMGE2 <- do.call("cbind", daily_TNHOMGE2)
monthly_TNHOMGE2 <- do.call("cbind", monthly_TNHOMGE2)
colnames(daily_TNHOMGE2) <- names(qcstat_HM)
colnames(monthly_TNHOMGE2) <- names(qcstat_HM)

########
### 
########

HMdata_dtn <- daily_TNHOMGE2
HMdata_dtx <- daily_TXHOMGE2
HMdata_mtn <- monthly_TNHOMGE2
HMdata_mtx <- monthly_TXHOMGE2
HM_stats <- stats_s

### saving data 

  #save(HMdata_dtn, HMdata_dtx, HMdata_mtn, HMdata_mtx, HM_stats,
  save(HMdata_mtn, HMdata_mtx, HMdata_dtn, HMdata_dtx, HM_stats,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step10_MHDATA_06.RData"))

  save(daily_TNHOMGE2, daily_TXHOMGE2, monthly_TNHOMGE2, monthly_TXHOMGE2, TXbreaks_monthly_values2,
       daily_TNHOMGE, daily_TXHOMGE, monthly_TNHOMGE, monthly_TXHOMGE, TNbreaks_monthly_values2,
       stats_s,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step10_HCDATA_06_analysis.RData"))
  