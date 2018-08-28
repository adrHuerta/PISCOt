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

###

if( get_os() == "windows" ) {
  
  load(file.path("G:","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step04_QCDATA_03.RData"))
  load("./data/shp_dataset.RData")
  ls()
} else if ( get_os() == "linux") {
  
  load(file.path("/media","buntu","TOSHIBA EXT","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step04_QCDATA_03.RData"))
  load("./data/shp_dataset.RData")
  ls()
}

###
qcstats_oro <- qcstats
qcstats <- subset(qcstats, XX > -74.5 & XX < -68 & YY < -12 & YY > -17.5) 

# Daily imputation of stations with more information : Homogenization purpose

###
statHM_tx <- apply(qcdata_tx, 2, function(x) get_large_data(data_base = xts(x, time(qcdata_tx)))) %>%
  .[. > 55] %>% 
  names()

statHM_tn <- apply(qcdata_tx, 2, function(x) get_large_data(data_base = xts(x, time(qcdata_tn)))) %>%
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

qcdata_tx <- qcdata_tx[, as.character(qcstats[match(statHM_tx, qcstats$CC),]$CC[!is.na(qcstats[match(statHM_tx, qcstats$CC),]$CC)]) ]
qcdata_tn <- qcdata_tn[, as.character(qcstats[match(statHM_tx, qcstats$CC),]$CC[!is.na(qcstats[match(statHM_tx, qcstats$CC),]$CC)]) ]

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


data_M_hmTX <- data_hmTX %>% 
  apply(., 2, function(z) apply.monthly(xts(z, time(.)), function(x) mean(x))) %>%
  xts(., seq(as.Date("1981-01-15"), as.Date("2015-12-15"), by = "month"))

data_M_hmTN <- data_hmTN %>% 
  apply(., 2, function(z) apply.monthly(xts(z, time(.)), function(x) mean(x))) %>%
  xts(., seq(as.Date("1981-01-15"), as.Date("2015-12-15"), by = "month"))


#######################

qcstat_HM <- qcstats[match(statHM_tx, as.character(qcstats$CC)),] %>%
  .[complete.cases(.), ] %>% 
  .[, c("XX","YY","CC")] %>%
  get_nearest_stat(nearest = T) %>%
  lapply(., function(x) get_dist_matrix(data_XY = x[, c("XX", "YY", "NN")]))

daily_TXHOMGE <- list()
TXbreaks_monthly_values <- list()
monthly_TXHOMGE <- list()

for(z in 1:length(qcstat_HM))
{
  
  mat_XY  <- qcstat_HM[[z]]
  dat_daily_XX <- data_hmTX[, paste("X", colnames(mat_XY)[1], sep = "")]
  dat_mat_XY <- data_M_hmTX[, paste("X", colnames(mat_XY), sep = "") ]
  
  res <- MW_phomog(data_base = dat_mat_XY,
                   data_XY = mat_XY,
                   daily_tTS = dat_daily_XX)
  
  daily_TXHOMGE[[z]] <- res$res_DHM
  monthly_TXHOMGE[[z]] <- res$res_HM
  
  TXbreaks_monthly_values[[z]] <- res$breks_noHM
}

daily_TXHOMGE <- do.call("cbind", daily_TXHOMGE)
colnames(daily_TXHOMGE) <- names(qcstat_HM)
# 
# write.zoo(daily_TXHOMGE, "G:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/daily_TXHOMGE.csv", quote = F, sep = ",")

monthly_TXHOMGE <- do.call("cbind", monthly_TXHOMGE)
colnames(monthly_TXHOMGE) <- names(qcstat_HM)
# write.zoo(monthly_TXHOMGE, "G:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/monthly_TXHOMGE.csv", quote = F, sep = ",")

names(TXbreaks_monthly_values) <- names(qcstat_HM)

TXbreaks_res <- lapply(TXbreaks_monthly_values, function(x) {
  res <- x[, c("location", "shift", "TIME")]
  colnames(res) <- c("Estación", "Magnitud", "Fecha")
  return(res)
  })

View(TXbreaks_res <- do.call("rbind", TXbreaks_res))

# colnames(data_hmTX) <- colnames(daily_TXHOMGE) 
# xyplot(data_hmTX, col = c("black")) +
#    as.layer(xyplot(daily_TXHOMGE, col = c("red")))

daily_TNHOMGE <- list()
TNbreaks_monthly_values <- list()
monthly_TNHOMGE <- list()

for(z in 1:length(qcstat_HM))
{
  
  mat_XY  <- qcstat_HM[[z]]
  dat_daily_XX <- data_hmTN[, paste("X", colnames(mat_XY)[1], sep = "")]
  dat_mat_XY <- data_M_hmTN[, paste("X", colnames(mat_XY), sep = "") ]
  
  res <- MW_phomog(data_base = dat_mat_XY,
                   data_XY = mat_XY,
                   daily_tTS = dat_daily_XX)
  
  daily_TNHOMGE[[z]] <- res$res_DHM
  monthly_TNHOMGE[[z]] <- res$res_HM
  TNbreaks_monthly_values[[z]] <- res$breks_noHM
}

daily_TNHOMGE <- do.call("cbind", daily_TNHOMGE)
colnames(daily_TNHOMGE) <- names(qcstat_HM)
# write.zoo(daily_TNHOMGE, "G:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/daily_TNHOMGE.csv", quote = F, sep = ",")

monthly_TNHOMGE <- do.call("cbind", monthly_TNHOMGE)
colnames(monthly_TNHOMGE) <- names(qcstat_HM)
# write.zoo(monthly_TNHOMGE, "G:/TRABAJOS/CLIMANDES_2/ENTREGABLES/8to_entregable_2017_2/fig/monthly_TNHOMGE.csv", quote = F, sep = ",")

names(TNbreaks_monthly_values) <- names(qcstat_HM)

# 
# colnames(data_hmTN)<- colnames(daily_TNHOMGE) 
# xyplot(data_hmTN, col = c("black")) +
#   as.layer(xyplot(daily_TNHOMGE, col = c("red")))

TNbreaks_res <- lapply(TNbreaks_monthly_values, function(x) {
  res <- x[, c("location", "shift", "TIME")]
  colnames(res) <- c("Estación", "Magnitud", "Fecha")
  return(res)
})
# 
# View(TNbreaks_res <- do.call("rbind", TNbreaks_res))
# 
# par(mfrow = c(2, 1))
# hist(as.numeric(substr(TXbreaks_res$Fecha,1, 4)), main = "")
# hist(as.numeric(substr(TNbreaks_res$Fecha,1, 4)), main = "")

# baseData <- data.frame(time = 1:420, coredata(data_M_hmTX[, paste("X", colnames(qcstat_HM[[1]]), sep = "") ]))
# baseData <- melt(baseData, id.vars = "time", variable.name = "location",
#                  value.name = "data")
# baseData$location<-gsub("X","",baseData$location)
# 
# out1 <- pairwiseSNHT(baseData, qcstat_HM[[1]], k=3, period=100,
#                      crit=qchisq(1-0.05/600,df=1), returnStat=F)
# colnames(out1)
# 
# plot(subset(baseData, location == "607")$data)
# plot(subset(out1$data, location == "607")$data)
# 
# plot(subset(out1$data, location == "607")$data - subset(baseData, location == "607")$data)



mat_XY  <- qcstat_HM[["776"]]
dat_daily_XX <- data_hmTX[, paste("X", colnames(mat_XY)[1], sep = "")]
dat_mat_XY <- data_M_hmTX[, paste("X", colnames(mat_XY), sep = "") ]


data_base = dat_mat_XY;
data_XY = mat_XY;
daily_tTS =dat_daily_XX;
Time_d = seq(as.Date("1981-01-15"), as.Date("2015-12-15"), by = "month");

par(mfrow = c(2, 1))
plot(as.zoo(daily_tTS), type = "p", cex = 0.5, ylim = c(8, 25), 
     xlab = "", ylab = "Diario")
plot(as.zoo(data_base[,1]), ylim = c(13, 21),
     xlab = "", ylab = "Mensual")

xyplot(as.zoo(dat_mat_XY), type = "l", col = "black")

baseData <- data.frame(time = 1:length(Time_d), coredata(data_base))
baseData <- melt(baseData, id.vars = "time", 
                 variable.name = "location",
                 value.name = "data")
baseData$location <- gsub("X" ,"" , baseData$location)

res_MW_phomog <- pairwiseSNHT(baseData, 
                              data_XY, 
                              k = 3, 
                              period = 100,
                              crit = qchisq(1-0.05/600, df = 1), 
                              returnStat = F)
res_noHM <- subset(baseData, location == colnames(data_XY)[1])$data
res_HM <- subset(res_MW_phomog$data, location == colnames(data_XY)[1])$data
breks_noHM <- subset(res_MW_phomog$breaks, location == colnames(data_XY)[1])
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

par(mfrow = c(1, 2))
plot(window(mothly_fact, start = "1994-07-01", end = "1995-01-01"), type = "o")
plot(window(splines_inter, start = "1994-07-01", end = "1995-01-01"), type = "o")

daily_tTS_HM <- daily_tTS + splines_inter

plot(as.zoo(cbind(res_noHM, res_HM)), type = "l",
     plot.type = "single", 
     main = paste("Monthly", colnames(data_XY)[1]),
     xlab = "", ylab = "", lwd = c(2, 2),
     col = c(1,2), cex = 0.1)


plot(as.zoo(cbind(daily_tTS, daily_tTS_HM)), type = "l",
     plot.type = "single", 
     main = paste("Daily", colnames(data_XY)[1]),
     xlab = "", ylab = "",
     col = c(1,2), cex = 0.1)


ress <- res_MW_phomog

trr <- apply(ress, 2, function(x) match(round(max(x, na.rm = T),2), round(x, 2)))
trr <- melt(t(trr)) %>%
  mutate(Var2 = factor(Var2, levels = colnames(res_MW_phomog)))

melt(ress) %>% 
  mutate(Var2 = factor(Var2, levels = colnames(res_MW_phomog))) %>%
  ggplot() + 
  geom_line(aes(x = Var1, y = value)) + 
  geom_vline(data = trr, aes(xintercept =  value), colour = "blue") +
  facet_wrap(~ Var2, scales = "free_y") + 
  geom_hline(yintercept =  qchisq(1-0.05/600, df = 1), colour = "red") + 
  theme_bw() + ylab("Estadistico SNHT") + xlab("Tiempo")

###################
#saving data to validation
daily_TXHOMGE
daily_TNHOMGE

xy_stat <- subset(qcstats_oro, DRE == 12 | DRE == 13 )[-match(c("669", "677", "140604"), subset(qcstats, DRE == 12 | DRE == 13 )$CC) ,]

data_index_2 <- match(xy_stat$CC, colnames(daily_TXHOMGE))
data_index_2 <- data_index_2[!is.na(data_index_2)]

daily_TXHOMGE_dd <- daily_TXHOMGE[, data_index_2]
daily_TNHOMGE_dd <- daily_TNHOMGE[, data_index_2]



data_index <- match( xy_stat$CC, colnames(qcdata_tx))

qcdata_tx_st <- qcdata_tx[, data_index]
qcdata_tn_st <- qcdata_tn[, data_index]


data_index_3 <- match(colnames(daily_TNHOMGE_dd), colnames(qcdata_tx_st))
data_index_3 <- data_index_3[!is.na(data_index_3)]

data_tx <- cbind(qcdata_tx_st[, -data_index_3], daily_TXHOMGE_dd)
data_tn <- cbind(qcdata_tn_st[, -data_index_3], daily_TNHOMGE_dd)

colnames(data_tx) <- gsub('X', '', colnames(data_tx))
colnames(data_tn) <- gsub('X', '', colnames(data_tn))

data_tx <- data_tx[, as.character(xy_stat$CC)]
data_tn <- data_tn[, as.character(xy_stat$CC)]

as.character(xy_stat$CC) == colnames(data_tx)
colnames(data_tn) == colnames(data_tx)


save(data_tn, 
     data_tx,
     xy_stat,
     file = "./examples/validation/observed_dataset.RData")


ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = xy_stat, 
             aes( x = XX, y = YY), 
             size = 1, colour = "blue") + 
  geom_text(data = xy_stat, 
            aes( x = XX, y = YY, label = CC) ) +
   coord_quickmap(ylim = c(-17.5, -11),
                 xlim = c(-75, -68.5)) + theme_bw() + xlab("") + ylab("")

