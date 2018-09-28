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

source('./functions/quality_control_temp.R')
source('./functions/imputation_functions.R')

###

  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step05_QCDATA_06_VS.RData"))
  load("./data/shp_dataset.RData")
  ls()

#######

qcdata_tx <- era_tx_CS_VS
qcdata_tn <- era_tn_CS_VS
qcstats_tx <- qc_stat_VS_tx
qcstats_tn <- qc_stat_VS_tn

#############################
########### TX
#############################


length_tx <- data.frame(n = qcdata_tx %>%
                          apply(., 2, function(x) filter_qc(xts(x, time(qcdata_tx)),  n_months = 10) ))  %>%
  mutate(grupo = ifelse(n >= 25, 1, 2) ) %>%
  cbind(., qcstats_tx)

ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = length_tx %>% subset(VSS == 1 & grupo == 1), 
             aes( x = XX, y = YY, colour = as.factor(GGR), shape = as.factor(grupo)), size = 3) + 
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_size_manual(values = c(2, 0.5)) +
  # geom_text_repel(data = subset(length_tx, CC == "687"),
  #                   aes( x = XX, y = YY, label = CC)) +
  coord_quickmap(ylim = c(-18, 0),
                 xlim = c(-81.6, -68)) + theme_bw() + xlab("") + ylab("") + 
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
                      values = c("black", "blue")) + 
  guides(fill=FALSE) +  theme_bw() + 
  xlab("") + ylab("Cantidad de datos")


nnames <- subset(length_tx, grupo == 1 & VSS == 1)[, c("XX", "YY", "CC")]$CC %>% as.character()
del_nnames <- subset(length_tx, grupo == 2 & VSS == 1)[, c("XX", "YY", "CC")]$CC %>% as.character()

data_hm <- get_nearest_stat(data_xy = length_tx[-match(del_nnames, length_tx$CC), c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat2(y, qcdata_tx)) %>%
  lapply(., function(y) std_dep_imputation(y))
  # do.call("cbind", .)

data_hm_model1 <- data_hm %>% lapply(function(z) z$model) %>% do.call("cbind", .)
data_hm <- data_hm %>% lapply(function(z) z$filled) %>% do.call("cbind", .)
qcdata_tx[, colnames(data_hm)] <- data_hm

nnames <- subset(length_tx,  grupo == 2 & VSS == 1)[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tx[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat2(y, qcdata_tx)) %>%
  lapply(., function(y) std_dep_imputation(y)) 

data_hm_model2 <- data_hm %>% lapply(function(z) z$model) %>% do.call("cbind", .)
data_hm <- data_hm %>% lapply(function(z) z$filled) %>% do.call("cbind", .)
qcdata_tx[, colnames(data_hm)] <- data_hm

qcdata_tx <- qcdata_tx[, subset(length_tx, VSS == 1) %>% .$CC ]
qcdata_model_tx <- cbind(data_hm_model1, data_hm_model2); qcdata_model_tx <- qcdata_model_tx[, subset(length_tx, VSS == 1) %>% .$CC ]

# for(xs in colnames(qcdata_tx)){
# 
# #  jpeg(paste("/media/buntu/D1AB-BCDE/databases/results/qc/infilling/tx/",xs, "_",".jpg", sep = ""), width = 700, height = 550)
#   plot(cbind(qcdata_tx[,xs], era_tx_CS_VS[,xs], qcdata_model_tx[,xs]) %>% zoo(), type = "p", cex = 0.1)
#   #  dev.off()
# 
# }

qcdata_tx[, "X139"] <- qcdata_model_tx[, "X139"]
qcdata_tx[, "X211"] <- qcdata_model_tx[, "X211"]
qcdata_tx[, "X219"] <- qcdata_model_tx[, "X219"]
qcdata_tx[, "X236"] <- qcdata_model_tx[, "X236"]
qcdata_tx[, "X250"] <- qcdata_model_tx[, "X250"]
qcdata_tx[, "X272"] <- qcdata_model_tx[, "X272"]
qcdata_tx[, "X282"] <- qcdata_model_tx[, "X282"]
qcdata_tx[, "X302"] <- qcdata_model_tx[, "X302"] #outlier
qcdata_tx[, "X340"] <- qcdata_model_tx[, "X340"] #outlier
qcdata_tx[, "X371"] <- qcdata_model_tx[, "X371"]
qcdata_tx[, "X378"] <- qcdata_model_tx[, "X378"]
qcdata_tx[, "X443"] <- qcdata_model_tx[, "X443"]
qcdata_tx[, "X805"] <- qcdata_model_tx[, "X805"]

#############################
########### TN
#############################

length_tn <- data.frame(n = qcdata_tn %>%
                          apply(., 2, function(x) filter_qc(xts(x, time(qcdata_tn)),  n_months = 10) ))  %>%
  mutate(grupo = ifelse(n >= 25, 1, 2) ) %>%
  cbind(., qcstats_tn)

ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = length_tn %>% subset(VSS = 1), 
             aes( x = XX, y = YY, colour = as.factor(GGR), shape = as.factor(grupo)), size = 3) + 
  scale_colour_manual(values = c("blue", "red", "green")) +
  scale_size_manual(values = c(2, 0.5)) +
  # geom_text_repel(data = subset(length_tx, CC == "687"),
  #                   aes( x = XX, y = YY, label = CC)) +
  coord_quickmap(ylim = c(-18, 0),
                 xlim = c(-81.6, -68)) + theme_bw() + xlab("") + ylab("") + 
  theme(legend.justification=c(0,0), legend.position=c(0.01,0)) 


length_tn[, c("CC", "grupo")] %>%
  mutate( grupo = factor(grupo, levels = c(1:4))) %>%
  by(., .$grupo, function(z){
    qcdata_tn[, as.character(z$CC)] %>%
      apply(., 1, function(t) sum(!is.na(t))) 
  }) %>% do.call("cbind", .) %>%
  xts(., time(qcdata_tn)) %>%
  autoplot(., facet = NULL) + 
  scale_colour_manual(guide=FALSE, 
                      values = c("black", "blue", "orange", "gray")) + 
  guides(fill=FALSE) +  theme_bw() + 
  xlab("") + ylab("Cantidad de datos")


nnames <- subset(length_tn, grupo == 1 & VSS == 1)[, c("XX", "YY", "CC")]$CC %>% as.character()
del_nnames <- subset(length_tn, grupo == 2 & VSS == 1)[, c("XX", "YY", "CC")]$CC %>% as.character()

data_hm <- get_nearest_stat(data_xy = length_tn[-match(del_nnames, length_tn$CC), c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat2(y, qcdata_tn)) %>%
  lapply(., function(y) std_dep_imputation(y)) 


data_hm_model1 <- data_hm %>% lapply(function(z) z$model) %>% do.call("cbind", .)
data_hm <- data_hm %>% lapply(function(z) z$filled) %>% do.call("cbind", .)
qcdata_tn[, colnames(data_hm)] <- data_hm

nnames <- subset(length_tn,  grupo == 2 & VSS == 1)[, c("XX", "YY", "CC")]$CC %>% as.character()
data_hm <- get_nearest_stat(data_xy = length_tn[, c("XX", "YY", "CC")],
                            nearest = F) %>%
  .[nnames] %>%
  lapply(., function(y) merge_nearest_stat2(y, qcdata_tn)) %>%
  lapply(., function(y) std_dep_imputation(y)) 

data_hm_model2 <- data_hm %>% lapply(function(z) z$model) %>% do.call("cbind", .)
data_hm <- data_hm %>% lapply(function(z) z$filled) %>% do.call("cbind", .)
qcdata_tn[, colnames(data_hm)] <- data_hm

qcdata_tn <- qcdata_tn[, subset(length_tn, VSS == 1) %>% .$CC ]
qcdata_model_tn <- cbind(data_hm_model1, data_hm_model2); qcdata_model_tn <- qcdata_model_tn[, subset(length_tn, VSS == 1) %>% .$CC ]
# 
# for(xs in 1:dim(qcdata_tn)[2]){
#  # jpeg(paste("/media/buntu/D1AB-BCDE/databases/results/qc/infilling/tn/",xs, "_",".jpg", sep = ""), width = 700, height = 550)
#   plot(cbind(qcdata_tn[,xs], era_tn_CS_VS[,xs], qcdata_model_tn[,xs]) %>% zoo(), type = "p", cex = 0.1)
#   Sys.sleep(1)
#  # dev.off()
# }

qcdata_tn[, "X176"] <- qcdata_model_tn[, "X176"]
#qcdata_tn[, "X253"] <- qcdata_model_tn[, "X253"] #outlier
qcdata_tn[, "X308"] <- qcdata_model_tn[, "X308"]
qcdata_tn[, "X319"] <- qcdata_model_tn[, "X319"]
qcdata_tn[, "X373"] <- qcdata_model_tn[, "X373"]
qcdata_tn[, "X387"] <- qcdata_model_tn[, "X387"]
qcdata_tn[, "X393"] <- qcdata_model_tn[, "X393"]
qcdata_tn[, "X554"] <- qcdata_model_tn[, "X554"]
qcdata_tn[, "X823"] <- qcdata_model_tn[, "X823"]


###### Saving data
######

Ddata_txx <- qcdata_tx
Dddata_tnn <- qcdata_tn
Ddstats_s <- subset(qc_stat_VS_tn, VSS == "1")

##### adding amount of n real_values 

n_rV <- data.frame(length_tx %>% subset(VSS == 1) %>% .[,c("n", "CC")], 
                   length_tn %>% subset(VSS == 1) %>% .[,c("n", "CC")]) %>%
  mutate(n_mean = (.[,1]+.[,3])/2) %>% .[,c("CC", "n_mean")] %>%
  merge(., Ddstats_s, by = c("CC")) %>% .[match(Ddstats_s$CC, .$CC),]

#####
Ddstats_s <- data.frame(Ddstats_s, n_mean = n_rV$n_mean)

all(Ddstats_s$CC == colnames(Ddata_txx))
all(Ddstats_s$CC == colnames(Dddata_tnn))

### saving data 

  save(Dddata_tnn, Ddata_txx, Ddstats_s,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step09_dailyDATA_06.RData"))
