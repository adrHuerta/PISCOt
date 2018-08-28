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

source('./functions/tools_get_os.R')
source('./functions/interpolation_functions.R')
source('./functions/quality_control_temp.R')

###

  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step10_MHDATA_06.RData"))
  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))
  ls()

  
    
#### 
  
  del_double_points <- function(list_of_points, 
                                data_set, 
                                is_it_coordinates = F)
  {
    
    if( is_it_coordinates == F ){
      
      stat_to_del <- lapply(list_of_points, function(x) x$CC %>% as.character) %>% unlist
      data_set_del <- data_set[, match(stat_to_del, colnames(data_set))]
      data_set_res <- data_set[, -match(stat_to_del, colnames(data_set))]
      stat_to_del_names <- c(colnames(data_set_res), 
                             lapply(list_of_points, function(x) x$CC %>% as.character %>% paste(., collapse = "")) %>% unlist)
      
      for(i in 1:length(list_of_points)){
        
        stat_to_del_p <- list_of_points[[i]]$CC %>% as.character()
        new_ts <- apply(data_set_del[, stat_to_del_p], 1, mean) %>% xts(., time(data_set_del))
        data_set_res <- cbind(data_set_res, new_ts)
      }
      
      colnames(data_set_res) <- stat_to_del_names
      return(data_set_res)
    } else {
      
      stat_to_del <- lapply(list_of_points, function(x) x$CC %>% as.character) %>% unlist
      data_set_del <- data_set[match(stat_to_del, data_set$CC), ]
      data_set_res <- data_set[-match(stat_to_del, data_set$CC), ]
      
      for(i in 1:length(list_of_points)){
        stat_to_del_p <- list_of_points[[i]]$CC %>% as.character()
        new_ts <- data_set_del[match(stat_to_del_p, data_set_del$CC), ]
        new_ts$CC[1] <- paste(stat_to_del_p, collapse = "")
        new_ts$XX[1] <- mean(new_ts$XX)
        new_ts$YY[1] <- mean(new_ts$YY)
        data_set_res <- rbind(data_set_res, new_ts[1,])
      }
      
      return(data_set_res)
    }
    
  }
  
#### reducing local temperature gradients
  
  #first iteration
  
  all_points0 <- get_nearest_stat(data_xy = HM_stats[, c("XX", "YY", "CC")],
                              nearest = T) %>%
    lapply(., function(y) subset(y, D < 11)) %>%
    .[lapply(., function(y) dim(y)[1]) != 1] 
  
  stats_all_points0 <- lapply(all_points0, function(z) z$NN)
  res_stats_all_points0 <- NULL
    for(i in 1:26){
      res_stats_all_points0[[i]]  <- lapply(stats_all_points0, function(x) setequal(x, stats_all_points0[[i]])) %>%
    .[. == T] 
  stats_all_points0[names(res_stats_all_points0)] <- NULL
  }
  
  res_stats_all_points0 <- res_stats_all_points0 %>% unique %>% 
    .[lapply(., function(x) length(x)) > 1] %>% 
    lapply(., function(x) {
      res <- all_points0[[names(x)[1]]]         # can other number
      colnames(res) <- c("XX","YY","CC","DD")
      return(res)
      })
  
  HM_stats_1 <- del_double_points(list_of_points = res_stats_all_points0,
                                  data_set = HM_stats,
                                  is_it_coordinates = T)
  rownames(HM_stats_1) <- NULL
  
  HMdata_dtn_del_1 <- del_double_points(list_of_points = res_stats_all_points0,
                                      data_set = HMdata_dtn)
  
  HMdata_dtx_del_1 <- del_double_points(list_of_points = res_stats_all_points0,
                                      data_set = HMdata_dtx)
  
  HMdata_mtn_del_1 <- del_double_points(list_of_points = res_stats_all_points0,
                                        data_set = HMdata_mtn)
  
  HMdata_mtx_del_1 <- del_double_points(list_of_points = res_stats_all_points0,
                                        data_set = HMdata_mtx)
  
  ### second iteration
  
  all_points1 <- get_nearest_stat(data_xy = HM_stats_1[, c("XX", "YY", "CC")],
                                  nearest = T) %>%
    lapply(., function(y) subset(y, D < 11)) %>%
    .[lapply(., function(y) dim(y)[1]) != 1] 
  
  stats_all_points1 <- lapply(all_points1, function(z) z$NN)
  res_stats_all_points1 <- NULL
  for(i in 1:2){
    res_stats_all_points1[[i]]  <- lapply(stats_all_points1, function(x) setequal(x, stats_all_points1[[i]])) %>%
      .[. == T] 
    stats_all_points1[names(res_stats_all_points1)] <- NULL
  }

  res_stats_all_points1 <- res_stats_all_points1 %>% unique %>% 
    .[lapply(., function(x) length(x)) > 1] %>% 
    lapply(., function(x) {
      res <- all_points1[[names(x)[1]]]         # can other number
      colnames(res) <- c("XX","YY","CC","DD")
      return(res)
    })
  
  HM_stats_2 <- del_double_points(list_of_points = res_stats_all_points1,
                                  data_set = HM_stats_1,
                                  is_it_coordinates = T)
  rownames(HM_stats_2) <- NULL
  
  HMdata_dtn_del_2 <- del_double_points(list_of_points = res_stats_all_points1,
                                        data_set = HMdata_dtn_del_1)
  
  HMdata_dtx_del_2 <- del_double_points(list_of_points = res_stats_all_points1,
                                        data_set = HMdata_dtx_del_1)
  
  HMdata_mtn_del_2 <- del_double_points(list_of_points = res_stats_all_points1,
                                        data_set = HMdata_mtn_del_1)
  
  HMdata_mtx_del_2 <- del_double_points(list_of_points = res_stats_all_points1,
                                        data_set = HMdata_mtx_del_1)
  ### Third iteration
  
  get_nearest_stat(data_xy = HM_stats_2[, c("XX", "YY", "CC")],
                                  nearest = T) %>%
    lapply(., function(y) subset(y, D < 11)) %>%
    .[lapply(., function(y) dim(y)[1]) != 1] 
  
  
#### revisiting if lat long points are inside of pixels
  
  xy_data <- HM_stats_2 %>%
    .[, c("CC", "XX", "YY")]
  coordinates(xy_data) <- ~XX+YY
  projection(xy_data) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  NA_xy_data <- xy_data %>% 
    check_NA_pixels(Rr = tx_lst[[1]], .) %>%
    .$NAmatch %>%
    changing_pixel_location(Rr = tx_lst[[1]], .) 
  
  if( exists("NA_xy_data") ){
    
    
    noNA_xy_data <- xy_data %>% 
      check_NA_pixels(Rr = tx_lst[[1]], .) %>%
      .$match 
    
    newXY_data <- rbind(noNA_xy_data, 
                        NA_xy_data) %>%
      .[ match(fdstats_s$CC, as.character(.$CC)) ,]
    rownames(newXY_data) <- NULL
    all(as.character(newXY_data$CC) == fdstats_s$CC)
    
    coordinates(newXY_data) <- ~XX+YY
    projection(newXY_data) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    
  }

#### revisiting if there are at least two lat lon point in the same pixel
  
xy_data %>% 
    extract(tx_lst[[1]], ., cellnumbers = T, sp = T) %>%
    data.frame() %>% 
    by(., .$cells, function(y) y) %>%
    .[lapply(., function(z) dim(z)[1]) > 1]

#### adding artifical points?

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))

r_raster <- tx_lst[[1]]

plot(r_raster)
plot(xy_data, add = T)
points_to_add <- click(r_raster, xy = T, n = 15, type = "o", id = T) %>%
  mutate(XX = x, YY = y, CC = paste("X", value, sep = ""),
         CC_NEW = NA, NN = NA, DRE = NA, ZZ = NA, ZZ_n = NA, 
         VSS = NA, GGR = NA) %>%
  .[,c("CC", "CC_NEW", "NN", "DRE", "ZZ","XX", "YY", "ZZ_n", "GGR", "VSS")]


ts_to_add <- split(points_to_add, seq(nrow(points_to_add))) %>%
  lapply(function(z) {
    
    stat_to_reduce <- xy_data[c(match(c("X176", "X281X282","X269", "X590","X0027GHCN",
                                        "X3308", "X401", "X211", "X382", "X387"), xy_data$CC)),] %>%
      as.data.frame()
    
    stat_to_reduce$dis <- distHaversine(c(z$XX, z$YY), 
                 stat_to_reduce[,c("XX","YY")], 
                 r=6378137)
    ts_res <- stat_to_reduce[order(stat_to_reduce$dis),] %>%
      .[1:2, "CC"] %>% as.character() 
    
    return(list(dtx =  HMdata_dtx_del_2[,ts_res] %>% apply(., 1, mean) %>% xts(., time(HMdata_dtx_del_2)) ,  
                dtn = HMdata_dtn_del_2[,ts_res] %>% apply(., 1, mean) %>% xts(., time(HMdata_dtx_del_2)), 
                mtx =  HMdata_mtx_del_2[,ts_res] %>% apply(., 1, mean) %>% xts(., time(HMdata_mtx_del_2)),  
                mtn = HMdata_mtn_del_2[,ts_res] %>% apply(., 1, mean) %>% xts(., time(HMdata_mtx_del_2)) ))
  })
  
ts_to_add_dtx <- lapply(ts_to_add, function(z) z[[1]]) %>%
  do.call(cbind, .)
  
ts_to_add_dtn <- lapply(ts_to_add, function(z) z[[2]]) %>%
  do.call(cbind, .)

ts_to_add_mtx <- lapply(ts_to_add, function(z) z[[3]]) %>%
  do.call(cbind, .)

ts_to_add_mtn <- lapply(ts_to_add, function(z) z[[4]]) %>%
  do.call(cbind, .)

colnames(ts_to_add_dtx) <- points_to_add$CC
colnames(ts_to_add_dtn) <- points_to_add$CC
colnames(ts_to_add_mtx) <- points_to_add$CC
colnames(ts_to_add_mtn) <- points_to_add$CC

## adding

HMdata_dtx_del_2 <- cbind(HMdata_dtx_del_2, ts_to_add_dtx)
HMdata_dtn_del_2 <- cbind(HMdata_dtn_del_2, ts_to_add_dtn)
HMdata_mtx_del_2 <- cbind(HMdata_mtx_del_2, ts_to_add_mtx)
HMdata_mtn_del_2 <- cbind(HMdata_mtn_del_2, ts_to_add_mtn)

HM_stats_2 <- rbind(HM_stats_2, points_to_add)
                    
####### #monthly obs values

 HMdata_mtx_del <- HMdata_dtx_del_2#  %>%
#   as.list() %>% 
#   lapply(function(z) apply.monthly(z, mean)) %>%
#   do.call(cbind, .) %>% xts(., seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month"))
# 
 HMdata_mtn_del <- HMdata_dtn_del_2 # %>%
  # as.list() %>%
  # lapply(function(z) apply.monthly(z, mean)) %>%
  # do.call(cbind, .) %>% xts(., seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "month"))

mclim_HMdata_tx <- 1:12 %>%
  formatC(., 1, flag = 0) %>%
  as.list() %>%
  lapply(., function(z) {
    
    HMdata_mtx_del %>%
      window(., end = "2010-12-31") %>%
      .[format(time(.), "%m") == z] %>%
      apply(., 2, mean) %>%
      round(., 1)
  }) %>%
  do.call("rbind", .) %>%
  t()
  

mclim_HMdata_tn <- 1:12 %>%
  formatC(., 1, flag = 0) %>%
  as.list() %>%
  lapply(., function(z) {
    HMdata_mtn_del %>%
      window(., end = "2010-12-31") %>%
      .[format(time(.), "%m") == z] %>%
      apply(., 2, mean) %>%
      round(., 1)
  }) %>%
  do.call("rbind", .) %>%
  t()

# mclim_HMdata_tn <- mclim_HMdata_tn[-match("X790", rownames(mclim_HMdata_tn)),]
# mclim_HMdata_tx <- mclim_HMdata_tx[-match("X790", rownames(mclim_HMdata_tx)),]
# HM_stats_2 <- HM_stats_2[-match("X790",HM_stats_2$CC ),]
# HMdata_dtn_del_2 <- HMdata_dtn_del_2[, -match("X790", colnames(HMdata_dtn_del_2))]
# HMdata_dtx_del_2 <- HMdata_dtx_del_2[, -match("X790", colnames(HMdata_dtx_del_2))]

###########
################################
###########

tx_obs_normal <- cbind(mclim_HMdata_tx , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(tx_obs_normal) <- ~XX+YY
projection(tx_obs_normal) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
names(tx_obs_normal) <- c( paste("To", 1:12, sep = ""), "CC")


tn_obs_normal <- cbind(mclim_HMdata_tn , HM_stats_2[,c("XX", "YY", "CC")] )
coordinates(tn_obs_normal) <- ~XX+YY
projection(tn_obs_normal) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
names(tn_obs_normal) <- c( paste("To", 1:12, sep = ""), "CC")

##############################
##########
###################

# ts_mclim_HMdata_tx <- apply(mclim_HMdata_tx, 1, mclim_dts)
# anom_dtx <- (((HMdata_dtx_del %>%
#                round(., 1)) - ts_mclim_HMdata_tx)/ts_mclim_HMdata_tx) %>%
#   t()
# colnames(anom_dtx) <- paste("TX_", gsub("-", "_", colnames(anom_dtx)),  sep = "")
# anom_dtx <- cbind(anom_dtx , HM_stats_del[,c("XX", "YY", "CC")] )
# coordinates(anom_dtx) <- ~XX+YY
# projection(anom_dtx) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#
# 
# ts_mclim_HMdata_tn <- apply(mclim_HMdata_tn, 1, mclim_dts)
# anom_dtn <- (((HMdata_dtn %>%
#                 round(., 1)) - ts_mclim_HMdata_tn)/ts_mclim_HMdata_tn) %>%
#   t()
# colnames(anom_dtn) <- paste("TN_", gsub("-", "_", colnames(anom_dtn)),  sep = "")
# anom_dtn <- cbind(anom_dtn , HM_stats_del[,c("XX", "YY", "CC")] )
# coordinates(anom_dtn) <- ~XX+YY
# projection(anom_dtn) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


### saving data 

  save(tx_obs_normal, tn_obs_normal,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","normal_obs_dataset.RData"))
  
  save(HMdata_dtx_del_2, HMdata_dtn_del_2, HM_stats_2, HMdata_mtx_del_2, HMdata_mtn_del_2,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_daily_obs_dataset.RData"))
