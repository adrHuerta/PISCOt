
MW_phomog <- function(data_base,
                      data_XY, 
                      daily_tTS,
                      Time_d = seq(as.Date("1981-01-15"), as.Date("2016-12-15"), by = "month"), ... )
{
  
  baseData <- data.frame(time = 1:length(Time_d), coredata(data_base))
  baseData <- melt(baseData, id.vars = "time", 
                   variable.name = "location",
                   value.name = "data")
  #baseData$location <- gsub("X" ,"" , baseData$location)
  
  res_MW_phomog <- pairwiseSNHT(baseData, 
                                data_XY, 
                                k = 3, 
                                period = 200,
                                crit = qchisq(1-0.05/600, df = 1), 
                                returnStat = F)
  
  res_noHM <- subset(baseData, location == colnames(data_XY)[1])$data
  res_HM <- subset(res_MW_phomog$data, location == colnames(data_XY)[1])$data
  breks_noHM <- subset(res_MW_phomog$breaks, colnames(data_XY)[1] == res_MW_phomog[[2]]$location )
  breks_noHM$TIME <- Time_d[breks_noHM$time]
  
  res_noHM <- xts(res_noHM, Time_d) 
  res_HM <- xts(res_HM, Time_d)
  breks_noHM <- breks_noHM  
  
  #return(list())
  
  mothly_fact <- res_HM - res_noHM
  
  y_values <- coredata(mothly_fact)
  x_values <- match( time(mothly_fact), time(daily_tTS))
  x_new <- 1:length(daily_tTS)
  
  
  splines_inter <- xts(aspline(x_values, y_values,x_new)$y,
                       time(daily_tTS))
  
  daily_tTS_HM <- daily_tTS + splines_inter
  
  plot(as.zoo(cbind(res_noHM, res_HM)), type = "l",
       plot.type = "single", 
       main = paste("Monthly", colnames(data_XY)[1]),
       xlab = "", ylab = "",
       col = c(1,2), cex = 0.1)
  
  plot(as.zoo(cbind(daily_tTS, daily_tTS_HM)), type = "l",
       plot.type = "single", 
       main = paste("Daily", colnames(data_XY)[1]),
       xlab = "", ylab = "",
       col = c(1,2), cex = 0.1)
  
  shifted_abs <- mothly_fact %>% .[ . != 0] 
  if( length(shifted_abs) == 0) {
    NULL
  } else {
    breks_noHM$shift_to_time <- round(shifted_abs,2) %>% factor() %>% levels()
    
  }
  
  return(list(res_noHM = res_noHM, 
              res_HM = res_HM, 
              breks_noHM = breks_noHM, 
              res_DHM = daily_tTS_HM))
  
  
}


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
    return(list(break_corr = res_HM-res_noHM))
    
  })
  
  res_v <-  lapply(res_seapha, function(z) z$break_corr) %>% do.call(rbind, .)
  
  return(list(TS_Corrected = res_v + monthlyTS, monthlyFacts = res_v))  
}


TOdailyHomg <- function(dailyTS = NULL,
                        monthlyFacts = NULL)
{
  
  y_values <- coredata(monthlyFacts)
  x_values <- match( time(monthlyFacts), time(dailyTS))
  x_new <- 1:length(dailyTS)
  
  
  splines_inter <- xts(aspline(x_values, y_values,x_new)$y,
                       time(dailyTS))
  
  return(list(TS_Corrected = dailyTS + splines_inter, dailyIntFacts = splines_inter))
  
}

seasonal_timeseries_r <- function(tss){
  
  names(tss) <- NULL 
  
  seasons_m <-  list( "Dec" = "12", "Jan" = "01", "Feb" = "02", "Mar" = "03", "Abr" ="04", "May" = "05",
                      "Jun" = "06", "Jul" = "07", "Aug" = "08", "Sep" = "09", "Oct" = "10", "Nov" = "11")

  sapply(seasons_m, function(z){
    

    res <- tss[format(time(tss), "%m") %in% z] %>% 
      apply.yearly(., mean) 

  }, USE.NAMES = TRUE, simplify = FALSE) 
  
}

seasonal_timeseries_rb <- function(tss){
  names(tss) <- NULL 
  seasons_m <- list( "DJF" = c("12","01","02"), "MAM" = c("03","04","05"), "JJA" = c("06","07","08"), "SON" = c("09","10","11"))
  sapply(seasons_m, function(z){
    
    if( any(z == "12") ){
      res_tss <- c(zoo(coredata( tss[12*1] ), time(tss)[1] - 31), 
                       tss, 
                       zoo(coredata( tss[ 12*(length(tss)/12) - 11] ), time(tss)[length(tss)] + 31), 
                       zoo(coredata( tss[ 12*(length(tss)/12) - 10] ),  time(tss)[length(tss)] + 62), 
                       zoo(coredata( tss[ 12*(length(tss)/12) - 9] ), time(tss)[length(tss)] + 90))
      res <- stats::lag(res_tss, -1)
      res <- res[format(time(res), "%m") %in% c("01","02","03")] 
      #res <- res[4:length(res)] 
      res <- res %>%  apply.yearly(., mean)
      
    } else {
      
      res <- tss[format(time(tss), "%m") %in% z] %>% 
        apply.yearly(., mean) 
    }
    
  }, USE.NAMES = TRUE, simplify = FALSE) 
  
  
  
  
}

seasonal_phab <- function(database_list = NULL,
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
    return(list(break_corr = res_HM-res_noHM))
    
  })
  
  res_v <-  lapply(res_seapha, function(z){
    months_t <- z[[1]][1] %>% time %>% format("%m")
    
    if ( months_t == "03" ){ 
      res <- monthlyTS[format(time(monthlyTS), "%m") %in% c("01","02","12")]
      coredata(res) <- rep(z$break_corr[-length(z$break_corr)], 3)
      return(res)
        
    } else if ( months_t == "05" ) {
      res <- monthlyTS[format(time(monthlyTS), "%m") %in% c("03","04","05")]
      coredata(res) <- rep(z$break_corr, 3)
      return(res)

    } else if (  months_t == "08"  ) {
      res <- monthlyTS[format(time(monthlyTS), "%m") %in% c("06","07","08")]
      coredata(res) <- rep(z$break_corr, 3)
      return(res)

    } else {
      res <- monthlyTS[format(time(monthlyTS), "%m") %in% c("09","10","11")]
      coredata(res) <- rep(z$break_corr, 3)
      return(res)
        
      }
    
  })  %>% do.call(rbind, .)
  
  return(list(TS_Corrected = res_v + monthlyTS, monthlyFacts = res_v))  
}
  