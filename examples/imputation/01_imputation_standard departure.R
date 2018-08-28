#following Thevakaran & Sonnadara (2017)
library(xts)
library(dplyr)
library(data.table)

rm(list = ls())

### source codes 

source('./functions/tools_get_os.R')

##############

if( get_os() == "windows" ) {
  
  load(file.path("G:", "DATABASES", "DATA", "PISCO_Temp_examples", "imputation", "data_tx.RData"))
  ls()
  
} else if ( get_os() == "linux") {
  
  load(file.path("/media","buntu","TOSHIBA EXT1", "DATABASES", "DATA", "PISCO_Temp_examples", "imputation", "data_tx.RData"))
  ls()
}

############

ts_c <- data_tx[,2]
ts_neg <- data_tx[,c(1,3,4)]

plot(ts_c, ts_neg[,1], xlim = c(10, 30), ylim = c(10,30))
plot(ts_c, ts_neg[,2], xlim = c(10, 30), ylim = c(10,30))
plot(ts_c, ts_neg[,3], xlim = c(10, 30), ylim = c(10,30))

####

mean(ts_c, na.rm = T); sd(ts_c, na.rm = T)
apply(ts_neg, 2, mean, na.rm = T); apply(ts_neg, 2, sd, na.rm = T)

####

ts_neg_standt <- ts_neg

for(i in 1:3){
  ts_neg_standt[,i] <- (ts_neg[,i] - mean(ts_neg[,i], na.rm = T))/sd(ts_neg[,i], na.rm = T)
}

weigths_ts <- c((1/(sd(ts_c, na.rm = T) - sd(ts_neg[,1], na.rm = T)))^2,
                (1/(sd(ts_c, na.rm = T) - sd(ts_neg[,2], na.rm = T)))^2,
                (1/(sd(ts_c, na.rm = T) - sd(ts_neg[,3], na.rm = T)))^2)


# ts_neg_standt_c <- (weigths_ts[1]*ts_neg_standt[,1] + 
#   weigths_ts[2]*ts_neg_standt[,2] + 
#   weigths_ts[3]*ts_neg_standt[,3])/(sum(weigths_ts))


ts_c_Res <- ts_c

for(i in 1:length(ts_c)){
  
  res <- rbind(ts_neg_standt[i,], weigths_ts)
  res_c <- res[ , colSums(is.na(res)) == 0]
  
  if( is.null(dim(res_c)) ){
    res_res <- res_c[1]
  } else {
    res_res <- sum(res_c[1,]*res_c[2,])/sum(res_c[2,])
  }
  
  
  ts_c_Res[i] <- res_res*sd(ts_c, na.rm = T) + mean(ts_c, na.rm = T)
}

res_c_c_Res <- data.frame(ts_c = ts_c, 
                          ts_c_Res = ts_c_Res)
res_c_c_Res_c <- res_c_c_Res[complete.cases(res_c_c_Res),]

plot(res_c_c_Res[,1])
plot(res_c_c_Res[,2])


res_c_c_Res <- transform(res_c_c_Res, new = 
                           ifelse(is.na(ts_c) & is.na(ts_c_Res), NA,
                                  ifelse(is.na(ts_c) & is.numeric(ts_c_Res), ts_c_Res, ts_c)))

plot(res_c_c_Res_c)
plot(res_c_c_Res[,3])