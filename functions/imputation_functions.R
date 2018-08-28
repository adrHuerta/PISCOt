get_large_data <- function(data_base, 
                           n_days = 20,
                           n_months = 12,
                           n_years = 2015-1981+1)
  {
  
  res_n_months <- apply.monthly(data_base, function(x)
  {
    
    if( sum(!is.na(x)) >= n_days )
    {
      return(1)
    } else {
      return(NA)
    }
    
  })
  
  res_n_years <- apply.yearly(res_n_months, function(z)
  {
    if( sum(!is.na(z)) >= n_months)
    {
      return(1)
    } else {
      return(NA)
    }
  })
  
  return(sum(res_n_years, na.rm = T)*100/n_years)
  
  
  
}


daily_imputation <- function(data_base)
  {
  
  res_data <- list()
  res_time <- list()
  
  for(i in 1:12)
  {
    
  res_database <- data_base[ as.numeric(format(time(data_base), "%m")) %in% i]
  res_database <- filling_data_spqc(res_database)
  res_data[[i]] <- coredata(res_database[,1])
  res_time[[i]] <- time(res_database[,1])
  
  }
  
  res_data <- unlist(res_data)
  res_time <- as.Date(unlist(res_time))
  
  return(xts(round(res_data, 2), res_time))
  
  }
  
get_dist_matrix <- function(data_XY)
  {
  
  res_XY <- list()
  
  for(j in 1:dim(data_XY)[1])
  {
    
    xy_c <- data_XY[j,c("XX","YY")]
    xy_v <- data_XY[ , c("XX","YY","NN")]
    res <- data.frame(xy_v, D = distHaversine(xy_c, xy_v[, c("XX", "YY")], r = 6378.137))
    
    res_xy <- res$D
    names(res_xy) <- res$NN
    res_XY[[j]] <- as.matrix(res_xy)
  }
  
  res_XY <- do.call("cbind", res_XY)
  colnames(res_XY) <- rownames(res_XY)
  
  return(res_XY)
}

##############################

std_dep_imputation <- function(data_base)
  {
  
  rest_all_orog <- list()
  rest_all_model <- list()
  rest_all_filled <- list()
  
  for(j in 1:12)
  {
    
    data_base_w <- data_base[ as.numeric(format(time(data_base), "%m")) %in% j]
    
    tc_st <- coredata(data_base_w[, 1])
    ta_st <- coredata(data_base_w[, -1])
    
    ts_neg_standt <- apply(ta_st, 2, function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T))
    weigths_ts <- apply(ta_st, 2, function(x) (1 / ( sd(tc_st, na.rm = T) - sd(x, na.rm = T) ) )^2 )
    
    tc_st_Res <- tc_st
    for(i in 1:length(tc_st)){
      
      res <- rbind(ts_neg_standt[i,], weigths_ts)
      res_c <- res[ , colSums(is.na(res)) == 0]
      
      if( is.null(dim(res_c)) ){
        res_res <- res_c[1]
      } else {
        res_res <- sum(res_c[1,]*res_c[2,])/sum(res_c[2,])
      }
      
      
      tc_st_Res[i] <- round(res_res*sd(tc_st, na.rm = T) + mean(tc_st, na.rm = T), 2)
    }
    
    res_c_c_Res <- data.frame(tc_st = as.numeric(tc_st), 
                              ts_c_Res = as.numeric(tc_st_Res))
    res_c_c_Res_c <- res_c_c_Res[complete.cases(res_c_c_Res),]
    
    res_c_c_Res <- transform(res_c_c_Res, new = 
                               ifelse(is.na(tc_st) & is.na(ts_c_Res), NA,
                                      ifelse(is.na(tc_st) & is.numeric(ts_c_Res), ts_c_Res, tc_st)))
    
    rest_all_orog[[j]] <- xts(res_c_c_Res$tc_st, time(data_base_w))
    rest_all_model[[j]] <- xts(res_c_c_Res$ts_c_Res, time(data_base_w))
    rest_all_filled[[j]] <- xts(res_c_c_Res$new, time(data_base_w))
    
  }
  
  rest_all <- list(orog = do.call("rbind", rest_all_orog), 
                   model = do.call("rbind", rest_all_model), 
                   filled = do.call("rbind", rest_all_filled))
    
  return(rest_all)
}

std_dep_imputation_seasonal <- function(data_base)
{
  
  rest_all_orog <- list()
  rest_all_model <- list()
  rest_all_filled <- list()
  
  seasonss <- list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
  
  for(j in 1:4)
  {
    
    data_base_w <- data_base[ as.numeric(format(time(data_base), "%m")) %in% seasonss[[j]]]
    
    tc_st <- coredata(data_base_w[, 1])
    ta_st <- coredata(data_base_w[, -1])
    
    ts_neg_standt <- apply(ta_st, 2, function(x) (x - mean(x, na.rm = T))/sd(x, na.rm = T))
    weigths_ts <- apply(ta_st, 2, function(x) (1 / ( sd(tc_st, na.rm = T) - sd(x, na.rm = T) ) )^2 )
    
    tc_st_Res <- tc_st
    for(i in 1:length(tc_st)){
      
      res <- rbind(ts_neg_standt[i,], weigths_ts)
      res_c <- res[ , colSums(is.na(res)) == 0]
      
      if( is.null(dim(res_c)) ){
        res_res <- res_c[1]
      } else {
        res_res <- sum(res_c[1,]*res_c[2,])/sum(res_c[2,])
      }
      
      
      tc_st_Res[i] <- round(res_res*sd(tc_st, na.rm = T) + mean(tc_st, na.rm = T), 2)
    }
    
    res_c_c_Res <- data.frame(tc_st = as.numeric(tc_st), 
                              ts_c_Res = as.numeric(tc_st_Res))
    res_c_c_Res_c <- res_c_c_Res[complete.cases(res_c_c_Res),]
    
    res_c_c_Res <- transform(res_c_c_Res, new = 
                               ifelse(is.na(tc_st) & is.na(ts_c_Res), NA,
                                      ifelse(is.na(tc_st) & is.numeric(ts_c_Res), ts_c_Res, tc_st)))
    
    rest_all_orog[[j]] <- xts(res_c_c_Res$tc_st, time(data_base_w))
    rest_all_model[[j]] <- xts(res_c_c_Res$ts_c_Res, time(data_base_w))
    rest_all_filled[[j]] <- xts(res_c_c_Res$new, time(data_base_w))
    
  }
  
  rest_all <- list(orog = do.call("rbind", rest_all_orog), 
                   model = do.call("rbind", rest_all_model), 
                   filled = do.call("rbind", rest_all_filled))
  
  return(rest_all)
}
###################### 

qmap_temp <- function(obs_ts, gridded_ts)
  {
   
  obs_ts <- apply(obs_ts, 1, mean, na.rm = T) %>%
      xts(., time(obs_ts))
  
  gridded_ts <- apply(gridded_ts, 1, mean, na.rm = T) %>%
    xts(., time(gridded_ts))
  
  res_ts <- cbind(obs_ts = obs_ts, 
                  gridded_ts = gridded_ts) %>%
    .[complete.cases(.), ] 
  
  
  mm <- c("01","02", "03", "04", "05","06", "07", "08","09", "10","11", "12")
  
  for(i in 1:length(mm)){
    
    res_mm <- res_ts[ format(time(res_ts), "%m") %in% mm[[i]] ]
    
    
    
    qm_fit <- fitQmapQUANT(coredata(res_mm$obs_ts), 
                           coredata(res_mm$gridded_ts) ,
                           qstep = 0.01, nboot = 1, wet.day = F, type = "tricube")
    
    coredata(res_ts$gridded_ts[ format(time(res_ts$gridded_ts), "%m") %in% mm[[i]] ]) <- doQmapQUANT(coredata(res_mm$gridded_ts[ format(time(res_mm$gridded_ts), "%m") %in% mm[[i]] ]), qm_fit, type="tricub") %>%
      as.numeric() %>%
      round(., 2)
    
    coredata(gridded_ts[ format(time(gridded_ts), "%m") %in% mm[[i]] ]) <- doQmapQUANT(coredata(gridded_ts[ format(time(gridded_ts), "%m") %in% mm[[i]] ]), qm_fit, type="tricub") %>%
      as.numeric() %>%
      round(., 2)
  }
  
  return(list(equal_data = res_ts, all_data = cbind(obs_ts = obs_ts, 
                                                    gridded_ts = gridded_ts) ))
  }

merge_nearest_stat2 <- function(data_xy, 
                               data_ts)
{
  
  stats_n <- data_xy$NN
  data_ts_merge <- data_ts[, as.character(stats_n)]
  colnames(data_ts_merge) <- as.character(stats_n)
  
  for(i in 3:length(stats_n)){
    
    res_r <- data_ts_merge[, 2:i]
    response <- is.element(0, apply(res_r, 1, function(x) sum(!is.na(x))))
    
    n_real_gauges <- length(colnames(res_r)) - length(grep( "VS", colnames(res_r))) 
    
    
    if( response == F & n_real_gauges == 3){
      break
    }
    
  }
  
  return(data_ts_merge[, 1:i])
  
}

#time_serie_xts <- dt_VS_tn[,3]
IQR_REA_ev <- function(time_serie_xts, umb = 3.5)
{
  
  #df_db <- time_serie_xts[complete.cases(time_serie_xts)]

  mm <- c("01","02", "03", "04", "05","06", "07", "08","09", "10","11", "12")
  res_values <- list()
  for(i in 1:length(mm)){
    
  df_db_i <- time_serie_xts[format(time(time_serie_xts), "%m") %in% mm[i]]  
  IQR_ts <- IQR(df_db_i %>% coredata())
  Q25_ts <- quantile(df_db_i %>% coredata(), .25)
  Q75_ts <- quantile(df_db_i %>% coredata(), .75)
  
  max_umb <- Q75_ts + umb*IQR_ts
  min_umb <- Q25_ts - umb*IQR_ts
  
  res_values[[i]] <- c(df_db_i[df_db_i >= max_umb ], df_db_i[df_db_i <= min_umb ])
    }
  
  res_values <- do.call(rbind, res_values)
  time_serie_xts[time(res_values)] <- NA; time_serie_xts_corrected <- time_serie_xts
  climatological_daily_values <- daily_clim_values(time_serie_xts)
  dates_to_fill <- match(format(time(res_values), "%m-%d"),  climatological_daily_values[,1])
  time_serie_xts_corrected[time(res_values)] <- climatological_daily_values[dates_to_fill,2] %>% unlist()
  
  
  return(list(noFilled = time_serie_xts, Filled = time_serie_xts_corrected, delete_values = res_values))
  
}

IQR_REA_seasonal <- function(time_serie_xts, umb = 2)
{
  
  #df_db <- time_serie_xts[complete.cases(time_serie_xts)]
  
  mm <- list(c("12","01","02"), c("03", "04", "05"), c("06", "07", "08"), c("09", "10","11"))
  res_values <- list()
  for(i in 1:4){
    
    df_db_i <- time_serie_xts[format(time(time_serie_xts), "%m") %in% mm[[i]]]  
    IQR_ts <- IQR(df_db_i %>% coredata())
    Q25_ts <- quantile(df_db_i %>% coredata(), .25)
    Q75_ts <- quantile(df_db_i %>% coredata(), .75)
    
    max_umb <- Q75_ts + umb*IQR_ts
    min_umb <- Q25_ts - umb*IQR_ts
    
    res_values[[i]] <- c(df_db_i[df_db_i >= max_umb ], df_db_i[df_db_i <= min_umb ])
  }
  
  res_values <- do.call(rbind, res_values)
  time_serie_xts[time(res_values)] <- NA; time_serie_xts_corrected <- time_serie_xts
  climatological_monthly_values <- monthly_clim_values(time_serie_xts)
  dates_to_fill <- match(format(time(res_values), "%m-%d"),  climatological_monthly_values[,1])
  time_serie_xts_corrected[time(res_values)] <- climatological_monthly_values[dates_to_fill,2] %>% unlist()
  
  
  return(list(noFilled = time_serie_xts, Filled = time_serie_xts_corrected, delete_values = res_values))
  
}

daily_clim_values <- function(time_serie_xts){
  
  time_serie_xts[format(time(time_serie_xts), "%m-%d") %in% "02-29"] -> res_to_del
  val_to_del <- match(time(res_to_del), time(time_serie_xts))
  res_to_29 <- time_serie_xts[-val_to_del] %>% 
    matrix(., nrow = 365) %>% 
    apply(., 1, mean, na.rm = T) 
  res_to_29 <- c(res_to_29[1:59], res_to_29[59], res_to_29[60:365])
    data.frame(day_clim = format(seq(as.Date("1984-01-01"), as.Date("1984-12-31"), by = "day"), "%m-%d"), 
               val_clim = res_to_29, stringsAsFactors = F)
  
}

monthly_clim_values <- function(time_serie_xts){
  
  
  monthly_clim <- time_serie_xts %>% 
    matrix(., nrow = 12) %>% 
    apply(., 1, mean, na.rm = T) 
  data.frame(monthly_clim = format(seq(as.Date("1984-01-15"), as.Date("1984-12-15"), by = "month"), "%m-%d"), 
             val_clim = monthly_clim, stringsAsFactors = F)
  
}
