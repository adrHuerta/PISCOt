rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(geosphere)
library(norm)
library(sp)
library(maptools)
library(rgdal)
library(ggplot2)
library(ggrepel)
library(snht)
library(akima)
### source codes 

source('./functions/tools_get_os.R')
source('./functions/imputation_functions.R')
source("./functions/quality_control_temp.R")

######

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step10_FDDATA_06.RData"))
ls()
# load(file.path("/media","buntu","TOSHIBA EXT1","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step04_QCDATA_03.RData"))
load("./data/shp_dataset.RData")
# ls()

####

qcstats_oro <- fdstats_s
qcstats <- subset(fdstats_s, XX > -74.5 & XX < -68 & YY < -12 & YY > -17.5) 

# Daily imputation of stations with more information : Homogenization purpose

###
statHM_tx <- apply(fddata_txx, 2, function(x) get_large_data(data_base = xts(x, time(fddata_txx)))) %>%
  .[. > 55] %>% 
  names()

statHM_tn <- apply(fddata_tnn, 2, function(x) get_large_data(data_base = xts(x, time(fddata_tnn)))) %>%
  .[. > 55] %>% 
  names()

statHM_tx == statHM_tn

ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = qcstats, 
             aes( x = XX, y = YY), 
             size = 1, colour = "blue") + 
  geom_point(data = qcstats[match(statHM_tx, as.character(qcstats$CC)),], 
             aes( x = XX, y = YY), 
             size = 2, colour = "darkred") + 
  geom_text_repel(data = qcstats, 
                  aes( x = XX, y = YY, label = CC), 
                  size = 2.5, colour = "black", fontface = 'bold') + 
  coord_quickmap(ylim = c(-17.5, -11),
                 xlim = c(-75, -68.5)) + theme_bw() + xlab("") + ylab("")

##############

fddata_txx <- fddata_txx[, as.character(qcstats[match(statHM_tx, qcstats$CC),]$CC[!is.na(qcstats[match(statHM_tx, qcstats$CC),]$CC)]) ]
fddata_tnn <- fddata_tnn[, as.character(qcstats[match(statHM_tx, qcstats$CC),]$CC[!is.na(qcstats[match(statHM_tx, qcstats$CC),]$CC)]) ]

##############
# 
# write.zoo(qcdata_tx, "F:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/qcdata_tx.csv", quote = F, sep = ",")
# write.zoo(qcdata_tn, "F:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/qcdata_tn.csv", quote = F, sep = ",")

data_hm <- get_nearest_stat(data_xy = qcstats[match(statHM_tx, as.character(qcstats$CC)),][complete.cases(qcstats[match(statHM_tx, as.character(qcstats$CC)),]), c("XX", "YY", "CC")],
                            nearest = T) 


data_hmTX <- lapply(data_hm, function(x) { 
  daily_imputation(data_base = qcdata_tx[, as.character(x$NN)])
}) %>% do.call("cbind", .)

data_hmTN <- lapply(data_hm, function(x) { 
  daily_imputation(data_base = qcdata_tn[, as.character(x$NN)])
})  %>% do.call("cbind", .)

# write.zoo(data_hmTX, "F:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/data_hmTX.csv", quote = F, sep = ",")
# write.zoo(data_hmTN, "F:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/data_hmTN.csv", quote = F, sep = ",")

data_M_hmTX <- fddata_txx %>% 
  apply(., 2, function(z) apply.monthly(xts(z, time(.)), function(x) mean(x))) %>%
  xts(., seq(as.Date("1981-01-15"), as.Date("2016-12-15"), by = "month"))

data_M_hmTN <- fddata_tnn %>% 
  apply(., 2, function(z) apply.monthly(xts(z, time(.)), function(x) mean(x))) %>%
  xts(., seq(as.Date("1981-01-15"), as.Date("2016-12-15"), by = "month"))

##############

qcstat_HM <- qcstats[match(statHM_tx, as.character(qcstats$CC)),] %>%
  .[complete.cases(.), ] %>% 
  .[, c("XX","YY","CC")] %>%
  get_nearest_stat(nearest = T) %>%
  lapply(., function(x) get_dist_matrix(data_XY = x[, c("XX", "YY", "NN")]))

### example 0

mat_XY  <- qcstat_HM[[1]]
dat_daily_XX <- fddata_txx[, colnames(mat_XY)[1] ]
dat_mat_XY <- data_M_hmTX[, colnames(mat_XY) ]

####
seasonal_timeseries_r <- function(tss){
  names(tss) <- NULL 
 # seasons_m <- list( "DJF" = c("12","01","02"), "MAM" = c("03","04","05"), "JJA" = c("06","07","08"), "SON" = c("09","10","11"))
  seasons_m <-  list( "Dec" = "12", "Jan" = "01", "Feb" = "02", "Mar" = "03", "Abr" ="04", "May" = "05",
                      "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11")
  sapply(seasons_m, function(z){
    
  #  if( any(z == "12") ){
      
      # res <- lag(tss, 1)
      # res <- res[format(time(res), "%m") %in% c("01","02","03")]
      # res <- res[4:length(res)]
      # res <- res %>%  apply.yearly(., mean)

    #} else {
      
      res <- tss[format(time(tss), "%m") %in% z] %>% 
        apply.yearly(., mean) 
  #  }
    
  }, USE.NAMES = TRUE, simplify = FALSE) 
  
  
  
  
}

all_seasonal_data <- data_M_hmTX %>% as.list() %>% lapply(seasonal_timeseries_r) 

### example 1

mat_XY  <- qcstat_HM[[1]]
# rownames(mat_XY) <- paste("X", rownames(mat_XY), sep = "")
# colnames(mat_XY) <- paste("X", colnames(mat_XY), sep = "")
dat_daily_XX <- fddata_txx[, colnames(mat_XY)[1]]
dat_mat_XY <- data_M_hmTX[, colnames(mat_XY) ]
all_seasonal_data <- dat_mat_XY %>% as.list() %>% lapply(seasonal_timeseries_r) 
all_seasonal_data_order <- list(x12 = lapply(all_seasonal_data, function(x) x$Dec) %>% do.call(cbind, .), 
                                x1 = lapply(all_seasonal_data, function(x) x$Jan) %>% do.call(cbind, .),
                                x2 = lapply(all_seasonal_data, function(x) x$Feb) %>% do.call(cbind, .),
                                x3 = lapply(all_seasonal_data, function(x) x$Mar) %>% do.call(cbind, .), 
                                x4 = lapply(all_seasonal_data, function(x) x$Abr) %>% do.call(cbind, .),
                                x5 = lapply(all_seasonal_data, function(x) x$May) %>% do.call(cbind, .),
                                x6 = lapply(all_seasonal_data, function(x) x$Jun) %>% do.call(cbind, .),
                                x7 = lapply(all_seasonal_data, function(x) x$Jul) %>% do.call(cbind, .),
                                x8 = lapply(all_seasonal_data, function(x) x$Aug) %>% do.call(cbind, .),
                                x9 = lapply(all_seasonal_data, function(x) x$Sep) %>% do.call(cbind, .),
                                x10 = lapply(all_seasonal_data, function(x) x$Oct) %>% do.call(cbind, .),
                                x11 = lapply(all_seasonal_data, function(x) x$Nov) %>% do.call(cbind, .)) 

dat_mat_XY <- mat_XY
database_list <- all_seasonal_data_order
###################

seasonal_pha <- function(database_list = NULL,
                         dat_mat_XY = NULL, 
                         monthlyTS = NULL){
  
  res_seapha <- lapply(database_list, function(j){
 #   for(j in 1:12) {
  #  j = database_list[[j]]
    data_XY <- j 
    baseData <- data.frame(time = 1:length(time(data_XY)), coredata(data_XY))
    baseData <- melt(baseData, id.vars = "time", 
                     variable.name = "location",
                     value.name = "data")

    #baseData$location <- gsub("X" ,"" , baseData$location)
    
    res_MW_phomog <- pairwiseSNHT(baseData, 
                                  dat_mat_XY, 
                                  k = 3, 
                                  period = 5,
                                  crit = qchisq(1-0.05/600, df = 1), 
                                  returnStat = F)
    
    res_noHM <- subset(baseData, location == colnames(data_XY)[1])$data
    res_HM <- subset(res_MW_phomog$data, location == colnames(data_XY)[1])$data
    
   # if(is.null(res_MW_phomog$breaks)) {
      
  #  }
    
   # breks_noHM <- subset(res_MW_phomog$breaks, location == colnames(data_XY)[1])
  #  breks_noHM$TIME <- time(data_XY)[breks_noHM$time]
    
    res_noHM <- xts(res_noHM, time(data_XY)) 
    res_HM <- xts(res_HM, time(data_XY))
  #  breks_noHM <- breks_noHM  
 #   }
   # return(list(break_inf = breks_noHM, break_corr = res_HM-res_noHM))
    return(list(break_inf = breks_noHM, break_corr = res_HM-res_noHM))
    
      })
    
    res_v <-  lapply(res_seapha, function(z) z$break_corr) %>% do.call(rbind, .)

    return(list(TS_Corrected = res_v + monthlyTS, monthlyFacts = res_v))  
  }


TOdailyHomg <- function(dailyTS = NULL,
                        monthlyFacts = NULL)
  {
  
  y_values <- monthlyFacts
  x_values <- match( time(monthlyFacts), time(dailyTS))
  x_new <- 1:length(dailyTS)
  
  
  splines_inter <- xts(aspline(x_values, y_values,x_new)$y,
                       time(dailyTS))
  
  return(list(TS_Corrected = dailyTS + splines_inter, dailyIntFacts = splines_inter))
  
  }
  

adradr <- seasonal_pha(database_list = all_seasonal_data_order,
             dat_mat_XY <- mat_XY, 
             monthlyTS = data_M_hmTX[, colnames(mat_XY) ][,1])

TOdailyHomg(dailyTS = dat_daily_XX,
            monthlyFacts = adradr$monthlyFacts )


plot(adradr$monthlyFacts %>% window(start = "2003-01-01", end = "2004-01-01"))
plot(TOdailyHomg(dailyTS = dat_daily_XX,
                 monthlyFacts = adradr$monthlyFacts)$dailyIntFacts %>% window(start = "2003-01-01", end = "2004-01-01"))

cbind(TOdailyHomg(dailyTS = dat_daily_XX,
                 monthlyFacts = adradr$monthlyFacts)$TS_Corrected, 
     dat_daily_XX) %>% zoo() %>% plot(plot.type = "single", lwd = 2, col = c(1,2))


cbind(adradr$TS_Corrected, 
      data_M_hmTX[, colnames(mat_XY) ][,1]) %>% zoo() %>% plot(plot.type = "single", lwd = 2, col = c(1,2))


### formal example

distSize  <- qcstat_HM[[40]]
dailytTEMP <- fddata_txx[, colnames(distSize)[1]]
monthlyTEMP <- data_M_hmTX[, colnames(distSize)[1]]
monthlyDB <- data_M_hmTX[, colnames(distSize) ]

SEASONAL_data <- monthlyDB %>% as.list() %>% lapply(seasonal_timeseries_r) 
SEASONAL_data_order <- list(x12 = lapply(SEASONAL_data, function(x) x$Dec) %>% do.call(cbind, .), 
                                x1 = lapply(SEASONAL_data, function(x) x$Jan) %>% do.call(cbind, .),
                                x2 = lapply(SEASONAL_data, function(x) x$Feb) %>% do.call(cbind, .),
                                x3 = lapply(SEASONAL_data, function(x) x$Mar) %>% do.call(cbind, .), 
                                x4 = lapply(SEASONAL_data, function(x) x$Abr) %>% do.call(cbind, .),
                                x5 = lapply(SEASONAL_data, function(x) x$May) %>% do.call(cbind, .),
                                x6 = lapply(SEASONAL_data, function(x) x$Jun) %>% do.call(cbind, .),
                                x7 = lapply(SEASONAL_data, function(x) x$Jul) %>% do.call(cbind, .),
                                x8 = lapply(SEASONAL_data, function(x) x$Aug) %>% do.call(cbind, .),
                                x9 = lapply(SEASONAL_data, function(x) x$Sep) %>% do.call(cbind, .),
                                x10 = lapply(SEASONAL_data, function(x) x$Oct) %>% do.call(cbind, .),
                                x11 = lapply(SEASONAL_data, function(x) x$Nov) %>% do.call(cbind, .)) 

#monthly PHA

adradr <- seasonal_pha(database_list = SEASONAL_data_order,
                       dat_mat_XY = distSize, 
                       monthlyTS = monthlyTEMP)

cbind(adradr$TS_Corrected,
     monthlyTEMP) %>% zoo() %>% plot(plot.type = "single", col = c(1,2), lwd = 2)

adradr_d <- TOdailyHomg(dailyTS = dailytTEMP,
              monthlyFacts = adradr$monthlyFacts )

cbind(adradr_d$TS_Corrected,
      dailytTEMP) %>% zoo() %>% plot(plot.type = "single", col = c(1,2), lwd = 2)

