enhanced_qc <- function(ts_temp){

  vec_months <- format(time(ts_temp), "%Y") %>% factor() %>% levels() %>% as.list()
  
  results_decim <- lapply(vec_months, function(z) {
    
    data_ts <- ts_temp[format(time(ts_temp), "%Y") == z]
    n_size <- sum(!is.na(data_ts))
    res_dec <- as.integer((data_ts %% 1)*10)
    
    if( n_size != 0 ){
      
      res  <- lapply(as.list(0:9), function(x){
        
        #asdddd %in% x/10
        round(length(which( res_dec %in% x ))*100/n_size, 2)
        
      }) %>% do.call("cbind", .)
      c(res, n_size*100/365)
      
    } else {
      
      c(rep(NA, 10), n_size*100/365)
      
    }
    
  })  

  results_decim <- do.call(rbind, results_decim)
  rownames(results_decim) <- vec_months
  colnames(results_decim) <- c(as.character(0:9), "n")
  #View(results_decim)
  
  ##### precision 
  
  results_decim_noNA <- results_decim[complete.cases(results_decim), ] %>% 
    .[,-c(11)] %>% data.frame()
  
  res_del_40 <- rowSums(results_decim_noNA >= 40) 
  res_del_00 <- rowSums(results_decim_noNA == 0) 
  res_del_35 <- rowSums(results_decim_noNA >= 35) 
  
  results_decim_noNA$res_del_40 <- res_del_40
  results_decim_noNA$res_del_00 <- res_del_00
  results_decim_noNA$res_del_35 <- res_del_35
  
  #### missingness
  
  results_decim_noNA <- data.frame(results_decim_noNA, 
                                   Ymiss = results_decim[complete.cases(results_decim), ] %>% .[,c(11)] ) 
  
  
  
  
  results_decim_noNA <- transform(results_decim_noNA, delete = ifelse(res_del_40 >= 1 , 
                                                                      1, ifelse(res_del_00 >= 5, 
                                                                                1, ifelse(res_del_35 >= 3 & res_del_00 >= 3, 
                                                                                          1, ifelse(Ymiss <= 30, 1 , 0)))))
  results_decim_noNA_Del <- subset(results_decim_noNA, delete != 0)
  ts_temp_new <- ts_temp
  ts_temp_new[format(time(ts_temp_new), "%Y") %in% rownames(results_decim_noNA_Del)] <- NA
  return(ts_temp_new)
  
  
  }

  
  
  

  
 










# rm(list = ls())
# 
# library(xts)
# library(dplyr)
# 
# prueba <- read.zoo("/media/buntu/D1AB-BCDE/databases/raw_data/ghcn/tmax_ghcn.txt", header = T, sep = ",", format = "%Y-%m-%d")
# asdr <- prueba[, 5]
# 
# ### point plots
# 
# plot(asdr, type = "p", cex = 0.1)
# 
# # ### decimal plots
# # 
# # apply.monthly(asdr, function(z){
# # 
# #   set.seed(200)
# #   asdddd <- round(rnorm(200),1)
# #   asdddda <- (asdddd %% 1)*10
# #   n_size_m <- length(asdddd)
# #   
# # res  <- lapply(as.list(0:9), function(x){
# #    
# #   #asdddd %in% x/10
# #   length(asdddda[asdddda %in% x])*100/n_size_m
# #   
# #  }) %>% do.call("cbind", .)
# #   
# #   
# # })
# 
# 
# vec_months <- format(time(asdr), "%Y") %>% factor() %>% levels() %>% as.list()
# 
# results_decim <- lapply(vec_months, function(z){
#   
#   data_ts <- asdr[format(time(asdr), "%Y") == z]
#   n_size <- sum(!is.na(data_ts))
#   res_dec <- as.integer((data_ts %% 1)*10)
#   
#   if( n_size != 0 ){
#     
#     res  <- lapply(as.list(0:9), function(x){
#       
#       #asdddd %in% x/10
#       round(length(which( res_dec %in% x ))*100/n_size, 2)
#       
#     }) %>% do.call("cbind", .)
#     c(res, n_size)
#     
#   } else {
#     
#     c(rep(NA, 10), n_size)
#     
#   }
#   
# 
#   
# 
# })
# 
# results_decim <- do.call(rbind, results_decim)
# rownames(results_decim) <- vec_months
# colnames(results_decim) <- c(as.character(0:9), "n")
# #View(results_decim)
# 
# results_decim_noNA <- results_decim[complete.cases(results_decim), ] %>% 
#   .[,-c(11)] %>% data.frame()
# 
#  
# res_del_40 <- rowSums(results_decim_noNA >= 40) 
# res_del_00 <- rowSums(results_decim_noNA == 0) 
# res_del_35 <- rowSums(results_decim_noNA >= 35) 
# 
# results_decim_noNA$res_del_40 <- res_del_40
# results_decim_noNA$res_del_00 <- res_del_00
# results_decim_noNA$res_del_35 <- res_del_35
# 
# results_decim_noNA <- transform(results_decim_noNA, delete = ifelse(res_del_40 >= 1 , 
#                                                                     T, ifelse(res_del_00 >= 5, 
#                                                                               T, ifelse(res_del_35 >= 3 & res_del_00 >= 3, 
#                                                                               1, 0))))
# results_decim_noNA_Del <- subset(results_decim_noNA, delete != 0)
# asdr_new <- asdr
# asdr_new[format(time(asdr_new), "%Y") %in% rownames(results_decim_noNA_Del)] <- NA
# 
# plot(asdr_new, type = "p", cex = 0.1)
# 


