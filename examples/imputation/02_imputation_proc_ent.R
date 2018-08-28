rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(ggplot2)
library(ggrepel)
library(sp)
library(maptools)
library(rgdal)
library(geosphere)

### source codes 

source('./functions/tools_get_os.R')
source('./functions/quality_control_temp.R')
source('./functions/imputation_functions.R')

###

if( get_os() == "windows" ) {
  
  load(file.path("G:","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step04_QCDATA_03.RData"))
  load("./data/shp_dataset.RData")
  ls()
  
} else if ( get_os() == "linux") {
  
  load(file.path("/media","buntu","TOSHIBA EXT1","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step04_QCDATA_03.RData"))
  load("./data/shp_dataset.RData")
  ls()
  
}

######
#stations to Cusco and Puno

qcstats_oro <- qcstats
qcdata_tx_oro <- qcdata_tx
qcdata_tn_oro <- qcdata_tn

qcstats <- subset(qcstats, XX > -74.5 & XX < -68 & YY < -12 & YY > -17.5) 
qcdata_tx <- qcdata_tx[, as.character(qcstats$CC)]
qcdata_tn <- qcdata_tn[, as.character(qcstats$CC)]

######

length_tx <- data.frame(n = qcdata_tx %>%
                          apply(., 2, function(x) filter_qc(xts(x, time(qcdata_tx))) ) ) %>%
  mutate(grupo = ifelse(n >= 20, 1, ifelse(n >= 15, 2, ifelse(n >= 10, 3, 4))) %>%
           as.factor() ) %>%
  cbind(., qcstats)

length_tn <- data.frame(n = qcdata_tn %>%
                          apply(., 2, function(x) filter_qc(xts(x, time(qcdata_tn))) ) ) %>%
  mutate(grupo = ifelse(n >= 20, 1, ifelse(n >= 15, 2, ifelse(n >= 10, 3, 4))) %>% 
           as.factor() ) %>%
  cbind(., qcstats)

####################
ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = length_tx, 
             aes( x = XX, y = YY), 
             size = 3) + 
  #scale_colour_manual(values = c("black", "blue", "orange", "gray")) +
  #geom_text_repel(data = qcstats, 
                  #aes( x = XX, y = YY, label = CC), 
                  #size = 2.5, colour = "black", fontface = 'bold') + 
  coord_quickmap(ylim = c(-18, -11),
                 xlim = c(-75.5, -68)) + theme_bw() + xlab("") + ylab("")


ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = length_tx, 
             aes( x = XX, y = YY, colour = grupo), 
             size = 4) + 
  scale_colour_manual(values = c("black", "blue", "orange", "gray")) +
  # geom_text_repel(data = subset(length_tx, CC == "687"),
  #                   aes( x = XX, y = YY, label = CC)) +
  coord_quickmap(ylim = c(-18, -11),
                 xlim = c(-75.5, -68)) + theme_bw() + xlab("") + ylab("") + 
  theme(legend.justification=c(0,0), legend.position=c(0.01,0)) 


length_tx[, c("CC", "grupo")] %>%
  mutate( grupo = factor(grupo, levels = c(1:4))) %>%
  by(., .$grupo, function(z){
    qcdata_tx[, as.character(z$CC)] %>%
      apply(., 1, function(t) sum(!is.na(t))) 
  }) %>% do.call("cbind", .) %>%
  xts(., time(qcdata_tx)) %>%
  autoplot(., facet = NULL) + 
  scale_colour_manual(guide=FALSE, 
                      values = c("black", "blue", "orange", "gray")) + 
  guides(fill=FALSE) +  theme_bw() + 
  xlab("") + ylab("Cantidad de datos")
  
View(length_tx)
####################
### example to inf

nnames <- subset(length_tx, grupo == "1")[, c("XX", "YY", "CC")]$CC %>% as.character()

data_hm <- get_nearest_stat(data_xy = length_tx[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .["687"] 

apply(merge_nearest_stat(data_hm$"687", qcdata_tx)[,-1], 1, function(x) sum(!is.na(x))) %>%
  plot(., ylim = c(0,21), ylab = "cantidad de datos", xlab = "dias")

data_hm <- get_nearest_stat(data_xy = length_tx[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .["687"] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tx)) 


### stations used


plot(zoo(data_hm$"687"), type = "l", cex = 0.1)
###

res <- std_dep_imputation(data_hm$"687")

###

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>%
  plot(xlim = c(9, 30), ylim = c(9, 30))

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>% 
  cor()

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>% 
  mutate(diff = Original_687 - Estimado_687) %>% 
  select(diff) %>%
  unlist() %>%
  hist( main = "", ylab = "Cantidad", xlab = "Original_687 - Estimado_687",
        xlim = c(-10, 10))

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>% 
  mutate(diff = Original_687 - Estimado_687) %>% 
  select(diff) %>% 
  unlist() %>%
  sd()

############

data.frame(Original_687 = res$orog, 
           Estimado_687 = res$model,
           Completado_687 = res$filled) %>%
  zoo(., time(res$orog)) %>%
  plot(main = "", type = "p", cex = 0.1, xlab = "años")


####################################

nnames <- subset(length_tx, grupo == "1")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tx[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tx)) %>%
  lapply(., function(y) std_dep_imputation(y)$filled) %>%
  do.call("cbind", .)

qcdata_tx[, gsub("X","",colnames(data_hm))] <- data_hm

nnames <- subset(length_tx, grupo == "2")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tx[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tx)) %>%
  lapply(., function(y) std_dep_imputation(y)$filled) %>%
  do.call("cbind", .)

qcdata_tx[, gsub("X","",colnames(data_hm))] <- data_hm

nnames <- subset(length_tx, grupo == "3")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tx[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tx)) %>%
  lapply(., function(y) std_dep_imputation(y)$filled) %>%
  do.call("cbind", .)

qcdata_tx[, gsub("X","",colnames(data_hm))] <- data_hm


nnames <- subset(length_tx, grupo == "4")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tx[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tx)) %>%
  lapply(., function(y) std_dep_imputation(y)$model) %>%
  do.call("cbind", .)

qcdata_tx[, gsub("X","",colnames(data_hm))] <- data_hm

###################################

data.frame(Or_815 = as.numeric(qcdata_tx_oro[,"815"]),
           Co_815 = as.numeric(qcdata_tx[,"815"]),
           Or_669 = as.numeric(qcdata_tx_oro[,"669"]),
           Co_669 = as.numeric(qcdata_tx[,"669"]),
           Or_140608 = as.numeric(qcdata_tx_oro[,"140608"]),
           Es_140608 = as.numeric(qcdata_tx[,"140608"])) %>%
  zoo(., time(qcdata_tx)) %>%
  plot(., main = "", type = "p", cex = 0.1, nc = 1)

write.zoo(qcdata_tx_oro[,colnames(qcdata_tx)], "/home/buntu/Desktop/copy/TX_original.csv", sep = ",",quote = F) 
write.zoo(qcdata_tx[,colnames(qcdata_tx)], "/home/buntu/Desktop/copy/TX_completada.csv", sep = ",",quote = F) 

##############################################

####################
ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = length_tx, 
             aes( x = XX, y = YY), 
             size = 3) + 
  #scale_colour_manual(values = c("black", "blue", "orange", "gray")) +
  #geom_text_repel(data = qcstats, 
  #aes( x = XX, y = YY, label = CC), 
  #size = 2.5, colour = "black", fontface = 'bold') + 
  coord_quickmap(ylim = c(-18, -11),
                 xlim = c(-75.5, -68)) + theme_bw() + xlab("") + ylab("")


ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = length_tn, 
             aes( x = XX, y = YY, colour = grupo), 
             size = 4) + 
  scale_colour_manual(values = c("black", "blue", "orange", "gray")) +
  # geom_text_repel(data = subset(length_tx, CC == "687"),
  #                   aes( x = XX, y = YY, label = CC)) +
  coord_quickmap(ylim = c(-18, -11),
                 xlim = c(-75.5, -68)) + theme_bw() + xlab("") + ylab("") + 
  theme(legend.justification=c(0,0), legend.position=c(0.01,0)) 


length_tn[, c("CC", "grupo")] %>%
  mutate( grupo = factor(grupo, levels = c(1:4))) %>%
  by(., .$grupo, function(z){
    qcdata_tx[, as.character(z$CC)] %>%
      apply(., 1, function(t) sum(!is.na(t))) 
  }) %>% do.call("cbind", .) %>%
  xts(., time(qcdata_tn)) %>%
  autoplot(., facet = NULL) + 
  scale_colour_manual(guide=FALSE, 
                      values = c("black", "blue", "orange", "gray")) + 
  guides(fill=FALSE) +  theme_bw() + 
  xlab("") + ylab("Cantidad de datos")

View(length_tn)
####################
### example to inf

nnames <- subset(length_tn, grupo == "1")[, c("XX", "YY", "CC")]$CC %>% as.character()

data_hm <- get_nearest_stat(data_xy = length_tn[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .["687"] 

apply(merge_nearest_stat(data_hm$"687", qcdata_tn)[,-1], 1, function(x) sum(!is.na(x))) %>%
  plot(., ylim = c(0,21), ylab = "cantidad de datos", xlab = "dias")

data_hm <- get_nearest_stat(data_xy = length_tn[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .["687"] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tn)) 


### stations used


plot(zoo(data_hm$"687"), type = "l", cex = 0.1)
###

res <- std_dep_imputation(data_hm$"687")

###

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>%
  plot(xlim = c(-5, 12), ylim = c(-5, 12))

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>% 
  cor()

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>% 
  mutate(diff = Original_687 - Estimado_687) %>% 
  select(diff) %>%
  unlist() %>%
  hist( main = "", ylab = "Cantidad", xlab = "Original_687 - Estimado_687",
        xlim = c(-10, 10))

data.frame(Original_687 = res$orog, Estimado_687 = res$model) %>%
  .[complete.cases(.), ] %>% 
  mutate(diff = Original_687 - Estimado_687) %>% 
  select(diff) %>% 
  unlist() %>%
  sd()

############

data.frame(Original_687 = res$orog, 
           Estimado_687 = res$model,
           Completado_687 = res$filled) %>%
  zoo(., time(res$orog)) %>%
  plot(main = "", type = "p", cex = 0.1, xlab = "años")


####################################

nnames <- subset(length_tn, grupo == "1")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tn[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tn)) %>%
  lapply(., function(y) std_dep_imputation(y)$filled) %>%
  do.call("cbind", .)

qcdata_tn[, gsub("X","",colnames(data_hm))] <- data_hm

nnames <- subset(length_tn, grupo == "2")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tn[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tn)) %>%
  lapply(., function(y) std_dep_imputation(y)$filled) %>%
  do.call("cbind", .)

qcdata_tn[, gsub("X","",colnames(data_hm))] <- data_hm

nnames <- subset(length_tn, grupo == "3")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tn[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tn)) %>%
  lapply(., function(y) std_dep_imputation(y)$filled) %>%
  do.call("cbind", .)

qcdata_tn[, gsub("X","",colnames(data_hm))] <- data_hm


nnames <- subset(length_tn, grupo == "4")[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tn[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat(y, qcdata_tn)) %>%
  lapply(., function(y) std_dep_imputation(y)$model) %>%
  do.call("cbind", .)

qcdata_tn[, gsub("X","",colnames(data_hm))] <- data_hm

write.zoo(qcdata_tn, "G:/TRABAJOS/CLIMANDES_2/ENTREGABLES/11vo_entr_2017_1_2/fig/TN_filled.csv", quote = F, sep = ",")
write.zoo(qcdata_tn_oro[,colnames(qcdata_tn)], "G:/TRABAJOS/CLIMANDES_2/ENTREGABLES/11vo_entr_2017_1_2/fig/TN_original.csv", quote = F, sep = ",")

###################################

data.frame(Or_815 = as.numeric(qcdata_tn_oro[,"815"]),
           Co_815 = as.numeric(qcdata_tn[,"815"]),
           Or_669 = as.numeric(qcdata_tn_oro[,"669"]),
           Co_669 = as.numeric(qcdata_tn[,"669"]),
           Or_140608 = as.numeric(qcdata_tn_oro[,"140608"]),
           Es_140608 = as.numeric(qcdata_tn[,"140608"])) %>%
  zoo(., time(qcdata_tn)) %>%
  plot(., main = "", type = "p", cex = 0.1, nc = 1)
