#Automatic Quality control

#====================== 1. General =============================

# - duplicated dates, comas, -999, -888 

duplicate_dates <- function(data_base, 
                            col_names = c("TT", "TN"))
  {
  
  duple_vals <- duplicated(data_base$TT)
  duple_vals_df <- data_base[data_base$TT %in% data_base$TT[duple_vals], ] 
  
  
  return(list(data_base = data_base[!duple_vals, ], 
              duple_vals = duple_vals_df))
  }

looking_wrong_dates <- function(data_base, 
                            col_names = c("TT", "TN"))
  {

  wrong_dates <- which(data_base$TT %in% NA)
  wrong_dates_df <- data_base[wrong_dates, ] 
  
  if ( length(wrong_dates) != 0) {
    
    return(list(data_base = data_base[-wrong_dates, ], 
                duple_vals = wrong_dates_df))
    
  } else {
    
    return(list(data_base =  data_base, 
                duple_vals = wrong_dates_df))
    
    
  }
  
  }


#====================== 2. Fixed ranges =============================
# - Tmax >= 60 | Tmax =< -10 
# - Tmin >= 40 | Tmin =< -30

fixed_ranges <- function(data_base, 
                         col_names = c("TT", "TN"), 
                         umb = c(min_val = -30, max_val = 40))
  {
  
  colnames(data_base) <- col_names
  
  filter_d <- paste(col_names[2], ">=",  umb["max_val"], "|", 
                    col_names[2], "<=",  umb["min_val"], sep =" ")
  
  data_del <- data_base %>% 
    filter_(filter_d)
  
  if( dim(data_del)[1] == 0){
    
    data_res <- data_base
    
  } 
  else 
    {
      data_res <- data_base[-match(data_del$TT, data_base$TT), ]
  }
  

  return(list(data_base = data_res, data_del = data_del))
}

#================== 3. Consistency among variables ======================
# - Tmax <= Tmin
# - Tmax = Tmin = 0

consisteny_amg_var <- function(data_base_tn, 
                               data_base_tx)
  {
  
  col_names = c("TT", "TN", "TX")
  
  colnames(data_base_tn) <- col_names[c(1,2)]
  colnames(data_base_tx) <- col_names[c(1,3)]
  
  tn_ts <- xts::xts(data_base_tn$TN, as.Date(data_base_tn$TT))
  tx_ts <- xts::xts(data_base_tx$TX, as.Date(data_base_tx$TT))
  
  tntx_ts <- cbind(tn_ts, tx_ts)
  colnames(tntx_ts) <- col_names[c(2,3)]
  
  filter_d <- paste("TX", "<=",  "TN", sep =" ")
  
  data_fr <- tntx_ts %>%
    as.data.table() 
  
  data_del <- data_fr %>%
    filter_(filter_d)
  
  if( dim(data_del)[1] == 0){
    
    data_res_TN <- data_fr %>%
      select(TT = index, TN)
    
    data_res_TX <- data_fr %>%
      select(TT = index, TX)
  } 
  else 
  {
    data_res <- data_fr[-match(data_del$index, data_fr$index), ]
    
    data_res_TN <- data_res %>%
      select(TT = index, TN)
    
    data_res_TX <- data_res %>%
      select(TT = index, TX)
  }
  
  
  return(list(data_base = list(data_res_TN = data_res_TN,
                               data_res_TX = data_res_TX),
              data_del = data_del))
  
}



#================== 3.3 IRQ extreme values =====================

IQR_ev <- function(data_base, umb = 3.5)
  {

  df_db <- data_base[complete.cases(data_base), ]
  colnames(df_db) <- c("TT", "VAL")
  
  IQR_ts <- IQR(df_db[, 2] %>% unlist())
  Q25_ts <- quantile(df_db[, 2] %>% unlist(), .25)
  Q75_ts <- quantile(df_db[, 2] %>% unlist(), .75)

  max_umb <- Q75_ts + umb*IQR_ts
  min_umb <- Q25_ts - umb*IQR_ts
  
  df_db <- transform(df_db, at = ifelse(VAL >= max_umb | VAL <= min_umb, "DEL", "OK"))
  df_db_del <- df_db %>% subset(at == "DEL") %>% select(TT, VAL)
  
  if( dim(df_db_del)[1] != 0){
  
    df_db_ok <- data_base[-match(df_db_del$TT, data_base$TT) ,]
    colnames(df_db_ok) <- colnames(data_base)
  } else {
    
    df_db_ok <- data_base
    
  }
  
  
  
  return(list(data_base = df_db_ok, data_del = df_db_del))
  
  }





# Filling dates 

filling_dates <- function(data_base, 
                          col_names = c("TT", "TN"), 
                          fill_date = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day"))
  {
  
  colnames(data_base) <- col_names
  
  res_date <- xts::xts(data_base[,2], as.Date(data_base$TT))
  res_date_filled <- merge(res_date, fill_date)
  
  return(res_date_filled)
  
}

#================== 4. Temporal Coherence ======================
# - Consecutive >8 days 


tempC_ccc <- function(data_base, 
                      umb = 8)
  {
  
  tt_res <- data.frame(tt = time(data_base),
                       t_temp = coredata(data_base),
                       t_val = sequence(rle(as.vector(coredata(data_base)))$lengths)) %>%
    .[.$t_val > umb, ]
  
  if( dim(tt_res)[1] != 0 ) {
    
    data_base[tt_res$tt] <- NA
    
    return(list(data_base = data_base, 
                data_del = tt_res))

    
  } else {
    
    return(list(data_base = data_base, 
                data_del = tt_res))
    
  }
  
}
 
# - Jumps >p99 of time serie of differences

tempC_jumps <- function(data_base, 
                        umb = 15)
{
  
  data_base_abs <- abs(data_base)
  data_base_abs_diff <- diff(data_base_abs)
  
  data_diff <- data_base_abs_diff[data_base_abs_diff > umb | data_base_abs_diff < -umb, ]
  #umb_del <- quantile(as.numeric(data_diff), perc_val)
  
  data_diff_q <- data_diff#
  
  if( length(data_diff_q) != 0){
    
    data_base[time(data_diff_q)] <- NA
    
    return(list(data_base = data_base, 
                data_del = data_diff_q))
    
  } else {
    
    return(list(data_base = data_base, 
                data_del = data_diff_q))
    
  }
  
}

#================== 5. Spatial Coherence ======================
# - Only done where area highly amount of stations (jungle may is not posible) [>3 stations]
# - Temperature to percentile values similar to Serrano et al 2010.

# Get nearest stations

get_nearest_stat <- function(data_xy, 
                             nearest = T)
  {
  
  colnames(data_xy) <- c("XX", "YY", "NN")
  n_stat <- dim(data_xy)[1]
  
  dit_XY <- list()
  
  for(i in 1:n_stat)
    {
    
    xy_c <- data_xy[i,c("XX","YY")]
    xy_v <- data_xy[, c("XX","YY")]
    res_xy <- data.frame(data_xy, D = distHaversine(xy_c, xy_v, r = 6378.137))
    res_xy <- res_xy[order(res_xy$D, decreasing = F), ]
    
    if( isTRUE(nearest) == T) {
      
      dit_XY[[i]] <- res_xy[1:11,]
      
    } else {
     
      dit_XY[[i]] <- res_xy
      
    }
    }

  names(dit_XY) <- data_xy[, c("NN")]
  
  return(dit_XY)
  
  }

# Merge data from nearest stations until specific condition

merge_nearest_stat <- function(data_xy, 
                               data_ts)
  {
  
  stats_n <- data_xy$NN
  data_ts_merge <- data_ts[, as.character(stats_n)]
  colnames(data_ts_merge) <- as.character(stats_n)
  
  for(i in 3:length(stats_n)){
    
    res_r <- data_ts_merge[, 2:i]
    response <- is.element(0, apply(res_r, 1, function(x) sum(!is.na(x))))
    
    n_real_gauges <- length(colnames(res_r)) - length(grep( "VS", colnames(res_r))) 
      
    
    if( response == F & n_real_gauges >= 6){
      break
    }
    
  }
  
  return(data_ts_merge[, 1:i])
  
  }

# Choosing stations with a highly amount of data

make_data_spqc <- function(data_base)
  {
  
  stat_cv <- data_base[,1]
  stat_cv_noNA <- time(stat_cv[!is.na(stat_cv)])
  data_base_noNA <- data_base[stat_cv_noNA]
  
  umb_d <- apply(data_base_noNA, 2, function(x){ 
     
      res <- sum(!is.na(x)) 
      
      if( res >= 0.65*length(data_base_noNA[,1]) ){ 
        return("SI")
      } else {
      return("NO")
    }  })
  
  return(data_base_noNA[, names(umb_d[umb_d == "SI"])])
  
}

# Imputation of missing data

  
filling_data_spqc <- function(data_base){
  
  rngseed(4324)
  
  mat <- coredata(data_base)
  
  pre <- prelim.norm(mat)
  mle <- em.norm(pre, showits = FALSE)
  
  mle.imputed <- imp.norm(pre, mle, mat)
  
  data_base <- xts(mle.imputed, time(data_base))
  
  return(data_base)
}

# Converting temp values to percentile values and spatial QC
# Deleting data above and below 78

spatial_qc <- function(data_base, 
                       umb = c(-78, 78))
  {
  
  mat <- coredata(data_base)
  
  if( dim(mat)[2] < 4){
    
    def_perc_ts_umb <- NULL
    return(def_perc_ts_umb)
    
  } else {
    
    mat_perc <- apply(mat, 2, function(x) ecdf(x)(x)*100)
    mat_perc_c <- mat_perc[,1]
    mat_perc_av <- apply(mat_perc[,-1], 1, mean)
    
    def_perc <- mat_perc_c - mat_perc_av
    def_perc_ts <- xts(def_perc, time(data_base))
    def_perc_ts_umb <- def_perc_ts[def_perc_ts > umb[2] | def_perc_ts < umb[1]]
    
    if( dim(def_perc_ts_umb)[1] == 0 ) {
      
      def_perc_ts_umb <- NULL
      return(def_perc_ts_umb)
      
    } else {
    
      return(def_perc_ts_umb)
      
        
    }
    
  }
  }

spatial_qc2 <- function(data_base, 
                       umb = c(-78, 78))
{
  
  mat <- coredata(data_base)

    mat_perc <- apply(mat, 2, function(x) ecdf(x)(x)*100)
    mat_perc_c <- mat_perc[,1]
    mat_perc_av <- apply(mat_perc[,-1], 1, mean)
    
    def_perc <- mat_perc_c - mat_perc_av
    def_perc_ts <- xts(def_perc, time(data_base))
    def_perc_ts_umb <- def_perc_ts[def_perc_ts > umb[2] | def_perc_ts < umb[1]]
    
      return(def_perc_ts_umb)
    }

#================= 6. Filter =====================

filter_qc <- function(data_base, 
                      n_days = 15,
                      n_months = 10,
                      monthlyTS = F)
  {
  
  res_n_months = data_base 

  if(monthlyTS == F) {
  
  res_n_months <- apply.monthly(data_base, function(x)
    {
    
    if( sum(!is.na(x)) >= n_days )
      {
      return(1)
      } else {
        return(NA)
      }
    
    })
  
  }
  
  res_n_years <- apply.yearly(res_n_months, function(z)
    {
    if( sum(!is.na(z)) >= n_months)
    {
      return(1)
    } else {
      return(NA)
    }
    })
  
  return(sum(res_n_years, na.rm = T))
  
  
  
}
