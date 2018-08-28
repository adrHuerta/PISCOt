  rm(list = ls())
  
  library(dplyr)
  library(data.table)
  library(xts)
  library(sp)
  library(maptools)
  library(rgdal)
  library(raster)
  library(ncdf4)
  library(qmap)
  
  ### source codes 
  
  source('./functions/quality_control_temp.R')
  source('./functions/imputation_functions.R')
  
  ###
  
  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step05_QCDATA_05.RData"))
  era_tx <- file.path("/media","buntu","D1AB-BCDE","databases","temporal_predictors","era_interim","tx","day_tx_1979_2016.nc")
  era_tn <- file.path("/media","buntu","D1AB-BCDE","databases","temporal_predictors","era_interim","tn","day_tn_1979_2016.nc")
  ls()
  
  ##############
  
  validation_stat_tx <- as.list(qc_tx) %>% lapply(., function(x) filter_qc(x, n_months = 10)) %>% unlist() %>% .[. >= 35] %>% names()
  validation_stat_tn <- as.list(qc_tn) %>% lapply(., function(x) filter_qc(x, n_months = 10)) %>% unlist() %>% .[. >= 35] %>% names()
  
  ##############
  
  donwscaled_tn <- as.list(qc_tn) %>% lapply(., function(x) filter_qc(x, n_months = 10)) %>% unlist() %>% .[. >= 20] %>% names()
  qc_stat %>% .[match(donwscaled_tn, .$CC),] %>% plot(YY~XX, .)
  
  donwscaled_tx <- as.list(qc_tx) %>% lapply(., function(x) filter_qc(x, n_months = 10)) %>% unlist() %>% .[. >= 20] %>% names()
  qc_stat %>% .[match(donwscaled_tx, .$CC),] %>% plot(YY~XX, .)

  ##############
  
  era_tn <- rotate(brick(era_tn)) 
  era_tx <- rotate(brick(era_tx)) 
  
  Xmap<-era_tx[[1]]
  Ymap<-era_tx[[1]]
  Xmap[,]<-coordinates(era_tx[[1]])[,1]
  Ymap[,]<-coordinates(era_tx[[1]])[,2]
  
  plot(Xmap)
  plot(Ymap)
  
  ##################### TX VS ########################
  
  xy_obs_tx <- qc_stat %>% .[match(donwscaled_tx, .$CC),] %>% .[, c("CC","XX", "YY", "GGR")]
  coordinates(xy_obs_tx) <- ~XX+YY
  projection(xy_obs_tx) <- projection(era_tx)
  
  pixel_post_tx <- extract(era_tx[[1]], xy_obs_tx, cellnumbers = T)[, "cells"] %>%
    data.frame(xy_obs_tx, pixel_point = paste("VS_", ., sep = ""), pixel_point_f = .) %>%
    by(., .$pixel_point_f, function(x) x) 
  
  dt_stat_VS_tx <- data.frame(ppp = pixel_post_tx %>% names() %>% as.numeric(),
                              plat = Ymap[names(pixel_post_tx) %>% as.numeric()],
                              plon = Xmap[names(pixel_post_tx) %>% as.numeric()],
                              CC = paste("VS_", names(pixel_post_tx), sep = ""),
                              CC_NEW = paste("VS_", names(pixel_post_tx), sep = ""),
                              NN = "NA",
                              DRE = NA, ZZ = NA, ZZ_n = NA,
                              YY = Ymap[names(pixel_post_tx) %>% as.numeric()],
                              XX =  Xmap[names(pixel_post_tx) %>% as.numeric()],
                              pp = names(pixel_post_tx) %>% as.numeric(),
                              GGR = lapply(pixel_post_tx, function(x) x[1,4]) %>% as.numeric(),
                              TT = "VS")
  
  dt_VS_tx <- era_tx[names(pixel_post_tx) %>% as.numeric()] %>% 
    t() %>%
    xts(., seq(as.Date("1979-01-01"), as.Date("2017-01-01"), by = "day")) %>%
    window(., start = "1981-01-01", end = "2016-12-31") 
  colnames(dt_VS_tx) <- paste("VS_", names(pixel_post_tx), sep = "")
  dt_VS_tx <- (dt_VS_tx - 273.5) 
  %>%
    apply(., 2, function(z) IQR_REA_ev(xts(z, time(.)))$Filled) %>%
    xts(., seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"))
  
  dt_noVS_tx <- lapply(pixel_post_tx, function(x) qc_tx[, x[1] %>% unlist() %>% as.character() ])
  names(dt_noVS_tx) <- paste("VS_", names(dt_noVS_tx) , sep = "")
  
  ###############
  
  xy_obs_tx_val <- qc_stat %>% .[match(validation_stat_tx, .$CC),] %>% .[, c("CC","XX", "YY", "GGR")]
  coordinates(xy_obs_tx_val) <- ~XX+YY
  projection(xy_obs_tx_val) <- projection(era_tx)
  
  pixel_post_tx_val <- extract(era_tx[[1]], xy_obs_tx_val, cellnumbers = T)[, "cells"] %>%
    data.frame(xy_obs_tx_val, pixel_point = paste("VS_", ., sep = ""), pixel_point_f = .) %>%
    by(., .$pixel_point_f, function(x) x) 
  
  dt_noVS_tx_val <- lapply(pixel_post_tx_val, function(x) qc_tx[, x[1] %>% unlist() %>% as.character() ])
  names(dt_noVS_tx_val) <- paste("VS_", names(dt_noVS_tx_val) , sep = "")
  
  
  ##################### TN VS ########################
  
  xy_obs_tn <- qc_stat %>% .[match(donwscaled_tn, .$CC),] %>% .[, c("CC","XX", "YY", "GGR")]
  coordinates(xy_obs_tn) <- ~XX+YY
  projection(xy_obs_tn) <- projection(era_tn)
  
  pixel_post_tn <- extract(era_tn[[1]], xy_obs_tn, cellnumbers = T)[, "cells"] %>%
    data.frame(xy_obs_tn, pixel_point = paste("VS_", ., sep = ""), pixel_point_f = .) %>%
    by(., .$pixel_point_f, function(x) x) 
  
  dt_stat_VS_tn <- data.frame(ppp = pixel_post_tn %>% names() %>% as.numeric(),
                              plat = Ymap[names(pixel_post_tn) %>% as.numeric()],
                              plon = Xmap[names(pixel_post_tn) %>% as.numeric()],
                              CC = paste("VS_", names(pixel_post_tn), sep = ""),
                              CC_NEW = paste("VS_", names(pixel_post_tn), sep = ""),
                              NN = "NA",
                              DRE = NA, ZZ = NA, ZZ_n = NA,
                              YY = Ymap[names(pixel_post_tn) %>% as.numeric()],
                              XX =  Xmap[names(pixel_post_tn) %>% as.numeric()],
                              pp = names(pixel_post_tn) %>% as.numeric(),
                              GGR = lapply(pixel_post_tn, function(x) x[1,4]) %>% as.numeric(),
                              TT = "VS")
  
  dt_VS_tn <- era_tn[names(pixel_post_tn) %>% as.numeric()] %>% 
    t() %>%
    xts(., seq(as.Date("1979-01-01"), as.Date("2017-01-01"), by = "day")) %>%
    window(., start = "1981-01-01", end = "2016-12-31") 
  colnames(dt_VS_tn) <- paste("VS_", names(pixel_post_tn), sep = "")
  dt_VS_tn <- (dt_VS_tn - 273.5)
  %>%
    apply(., 2, function(z) IQR_REA_ev(xts(z, time(.)))$Filled) %>%
    xts(., seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"))
    
  dt_noVS_tn <- lapply(pixel_post_tn, function(x) qc_tn[, x[1] %>% unlist() %>% as.character() ])
  names(dt_noVS_tn) <- paste("VS_", names(dt_noVS_tn) , sep = "")
  
  #################
  
  xy_obs_tn_val <- qc_stat %>% .[match(validation_stat_tn, .$CC),] %>% .[, c("CC","XX", "YY", "GGR")]
  coordinates(xy_obs_tn_val) <- ~XX+YY
  projection(xy_obs_tn_val) <- projection(era_tx)
  
  pixel_post_tn_val <- extract(era_tn[[1]], xy_obs_tn_val, cellnumbers = T)[, "cells"] %>%
    data.frame(xy_obs_tn_val, pixel_point = paste("VS_", ., sep = ""), pixel_point_f = .) %>%
    by(., .$pixel_point_f, function(x) x) 
  
  dt_noVS_tn_val <- lapply(pixel_post_tn_val, function(x) qc_tn[, x[1] %>% unlist() %>% as.character() ])
  names(dt_noVS_tn_val) <- paste("VS_", names(dt_noVS_tn_val) , sep = "")
  
  # ############# DTR ####
  # 
  # dt_VS_dtr <- dt_VS_tx - dt_VS_tn
  # 
  # dt_noVS_dtr <- lapply(pixel_post_tn, function(x) {
  #   qc_tx[, x[1] %>% unlist() %>% as.character() ] - qc_tn[, x[1] %>% unlist() %>% as.character() ] 
  #   })
  # names(dt_noVS_dtr) <- paste("VS_", names(dt_noVS_dtr) , sep = "")
  # ###
  # 
  # plot(cbind(dt_VS_tx[, "VS_30225"], dt_noVS_tx[["VS_30225"]]) %>%
  #        as.zoo())
  # 
  # plot(cbind(dt_VS_tn[, "VS_30225"], dt_noVS_tn[["VS_30225"]]) %>%
  #        as.zoo())
  # 
  # plot(cbind(dt_VS_dtr[, "VS_30225"], dt_noVS_dtr[["VS_30225"]]) %>%
  #        as.zoo())
  # 
  # plot( qmap_temp(obs_ts = dt_noVS_tn[["VS_26368"]],
  #                 gridded_ts = dt_VS_tn[, "VS_26368"])$all_data %>%
  #         zoo() )
  # 
  # plot( qmap_temp(obs_ts = dt_noVS_tx[["VS_11365"]],
  #                 gridded_ts = dt_VS_tx[, "VS_11365"])$all_data %>%
  #         zoo() )
  # 
  # plot( qmap_temp(obs_ts = dt_noVS_dtr[["VS_12601"]],
  #                 gridded_ts = dt_VS_dtr[, "VS_12601"])$all_data %>%
  #         zoo() )
  # 
  # ##############
  
  era_tn_uqp_qmap <- names(dt_noVS_tn) %>%
    as.list() %>%
    lapply(., function(zzz) qmap_temp(obs_ts = dt_noVS_tn[[ zzz]], 
                                    gridded_ts = dt_VS_tn[, zzz] )$all_data$gridded_ts ) %>%
    do.call("cbind", .)
  
  colnames(era_tn_uqp_qmap) <- names(dt_noVS_tn)
  
  # era_dtr_uqp_qmap <- names(dt_noVS_dtr) %>%
  #   as.list() %>%
  #   lapply(., function(zzz) qmap_temp(obs_ts = dt_noVS_dtr[[ zzz]], 
  #                                     gridded_ts = dt_VS_dtr[, zzz] )$all_data$gridded_ts ) %>%
  #   do.call("cbind", .)
  # 
  # colnames(era_dtr_uqp_qmap) <- names(dt_noVS_dtr)
  # 
  # era_tx_uqp_qmap <- era_dtr_uqp_qmap + era_tn_uqp_qmap
  # 
  era_tx_uqp_qmap <- names(dt_noVS_tx) %>%
    as.list() %>%
    lapply(., function(zzz) qmap_temp(obs_ts = dt_noVS_tx[[ zzz]],
                                    gridded_ts = dt_VS_tx[, zzz] )$all_data$gridded_ts ) %>%
    do.call("cbind", .)

  colnames(era_tx_uqp_qmap) <- names(dt_noVS_tx)
  
  ################
  
  qc_stat_VS_tn <- rbind(data.frame(qc_stat, VSS = 1),  data.frame(dt_stat_VS_tn[, colnames(qc_stat)], VSS = 2))
  qc_stat_VS_tx <- rbind(data.frame(qc_stat, VSS = 1),  data.frame(dt_stat_VS_tx[, colnames(qc_stat)], VSS = 2))
  
  ###############
  
  era_tx_CS_VS <- cbind(qc_tx, era_tx_uqp_qmap)
  era_tn_CS_VS <- cbind(qc_tn, era_tn_uqp_qmap)
  #era_tx_CS_VS <- cbind(qc_tx, era_tx_uqp_qmap)
  
  #############
  all(qc_stat_VS_tx$CC == colnames(era_tx_CS_VS))
  all(qc_stat_VS_tn$CC == colnames(era_tn_CS_VS))
  
  #############
  
  Mera_tx_CS_VS <- era_tx_CS_VS %>% 
    apply(., 2, function(z) apply.monthly(xts(z, time(.)), function(x){
      if(sum(is.na(x)) > 15){
        return(NA)
      } else {
        mean(x, na.rm = T)
      }
    } )) %>%
    xts(., seq(as.Date("1981-01-15"), as.Date("2016-12-15"), by = "month"))
  
  Mera_tn_CS_VS <- era_tn_CS_VS %>% 
    apply(., 2, function(z) apply.monthly(xts(z, time(.)), function(x){
      if(sum(is.na(x)) > 15){
        return(NA)
      } else {
          mean(x, na.rm = T)
      }
    } )) %>%
    xts(., seq(as.Date("1981-01-15"), as.Date("2016-12-15"), by = "month"))

  ### saving data 
  
    save(era_tx_CS_VS, era_tn_CS_VS, qc_stat_VS_tx, qc_stat_VS_tn,
         Mera_tx_CS_VS, Mera_tn_CS_VS,
         file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step05_QCDATA_06_VS.RData"))
    
    save(dt_noVS_tx, dt_VS_tx, 
         dt_noVS_tn, dt_VS_tn,
         qc_stat_VS_tx, qc_stat_VS_tn,
         validation_stat_tx, validation_stat_tn,
         dt_noVS_tx_val, dt_noVS_tn_val,
         file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step05_QCDATA_06_VS_analysis.RData"))
    