load(file.path("/media","buntu","TOSHIBA EXT1","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step06_QCDATA_05.RData"))
load(file.path("/media","buntu","TOSHIBA EXT1","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step05_QCDATA_04.RData"))

load("./data/shp_dataset.RData")
ls()


for(i in colnames(HMdata_tn)){
  
  plot(cbind(HMdata_tn[, i],fddata_tnn[, paste("X",i, sep = "")]) %>% as.zoo())
  Sys.sleep(0.5)
}


######

kriss_tn <- "/media/buntu/TOSHIBA EXT1/DATABASES/DATA/TEMPERATURE_OBSDATASET/others/datos_homogeneos_snts-adrian.csv" %>%
  read.csv(., header = T, na.strings = "") %>% 
  .[!is.na(.$temperatura), ] %>%
  .$codigo %>%
  paste("X", ., sep = "") %>%
  match(., fdstats_s$CC) %>%
  .[!is.na(.)] %>%
  HMdata_tn[, .] %>%
  apply(., 2, function(z) {
    z <- xts(z, time(.))
    res <- apply.monthly(z, function(h) mean(h))
    res <- round(res, 2)
    return(res)
  }) %>%
  xts(., seq(as.Date("1981-01-01"), as.Date("2015-12-01"), by = "month")) %>%
  window(., end = "2014-12-01") %>%
  write.zoo(., "kriss_tn.csv", quote = F, sep = ",")

kriss_tx <- "/media/buntu/TOSHIBA EXT1/DATABASES/DATA/TEMPERATURE_OBSDATASET/others/datos_homogeneos_snts-adrian.csv" %>%
  read.csv(., header = T, na.strings = "") %>% 
  .[!is.na(.$temperatura), ] %>%
  .$codigo %>%
  paste("X", ., sep = "") %>%
  match(., fdstats_s$CC) %>%
  .[!is.na(.)] %>%
  HMdata_tx[, .] %>%
  apply(., 2, function(z) {
    z <- xts(z, time(.))
    res <- apply.monthly(z, function(h) mean(h))
    res <- round(res, 2)
    return(res)
  }) %>%
  xts(., seq(as.Date("1981-01-01"), as.Date("2015-12-01"), by = "month")) %>%
  window(., end = "2014-12-01")  %>%
  write.zoo(., "kriss_tx.csv", quote = F, sep = ",")


############3

"/media/buntu/TOSHIBA EXT1/DATABASES/DATA/TEMPERATURE_OBSDATASET/others/datos_homogeneos_snts-adrian.csv" %>%
  read.csv(., header = T, na.strings = "") %>% 
  .[!is.na(.$temperatura), ] %>%
  .$codigo %>%
  paste("X", ., sep = "") %>%
  match(., fdstats_s$CC) %>%
  fdstats_s[., ] 


###################