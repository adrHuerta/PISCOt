rm(list = ls())

library(data.table)
library(dplyr)
library(xts)
library(rgbif)
library(ClustGeo)
library(ggplot2)
library(raster)

source('./functions/quality_control_temp.R')


##

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step05_QCDATA_04.RData"))
ls()

######

# get_nearest_stat(data_xy = raw_spat_St_eqc[, c("XX", "YY", "CC")],
#                  nearest = T)
# 
# plot(raw_spat_St_eqc[,"XX"], raw_spat_St_eqc[,"YY"])
# points(raw_spat_St_eqc[match(adr3, raw_spat_St_eqc$CC),][,"XX"],
#        raw_spat_St_eqc[match(adr3, raw_spat_St_eqc$CC),][,"YY"], pch = 4,cex = 5)
######

# is goood elevation data?

# apikey <- getOption("g_elevation_api")
# key <- name_suggest('Puma concolor')$key[1]
# 
# elv_tx <- raw_spat_St_eqc_tx[, c(6,7)]; colnames(elv_tx) <- c("decimalLongitude","decimalLatitude")
# elv_tn <- raw_spat_St_eqc_tn[, c(6,7)]; colnames(elv_tn) <- c("decimalLongitude","decimalLatitude")

# elv_tx <- elv_tx %>% elevation(., key = apikey) %>% .[, 3]
# elv_tn <- elv_tn %>% elevation(., key = apikey) %>% .[, 3]

elv <- file.path("/media", "buntu", "D1AB-BCDE", "databases", "spatial_predictors","dem_and_others3","GMTED2010_gee.tif") %>% #dem.tif
  raster()
names(elv) <- c("dem")
raster_dem <- as.data.frame(elv,  xy = T)
raster_dem$dem[raster_dem$dem <= 0] <- NA

elv_tx <- raw_spat_St_eqc[, c(6,7)]
coordinates(elv_tx) <- ~XX+YY
proj4string(elv_tx) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


plot(extract(elv, elv_tx),raw_spat_St_eqc$ZZ)

raw_spat_St_eqc$ZZ_n <- extract(elv, elv_tx)

# what about sea distance?

ditsea <- raster("/media/buntu/D1AB-BCDE/databases/spatial_predictors/dem_and_others/distSea.tif")

ditsea_tx <- raw_spat_St_eqc[, c(6,7)]
coordinates(ditsea_tx) <- ~XX+YY
proj4string(ditsea_tx) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

raw_spat_St_eqc$distsea <- extract(ditsea, ditsea_tx)


# Applying Ward Hierarchical Clustering

mean_TN <- raw_temp_TN_7 %>% 
  apply(., 2, apply.monthly, mean, na.rm = T) %>% 
  xts(., seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month")) %>% 
  apply(., 2, apply.yearly, mean, na.rm = T) %>% 
  apply(., 2, mean, na.rm = T) 

mean_TX <- raw_temp_TX_7 %>% 
  apply(., 2, apply.monthly, mean, na.rm = T) %>% 
  xts(., seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month")) %>% 
  apply(., 2, apply.yearly, mean, na.rm = T) %>% 
  apply(., 2, mean, na.rm = T) 

mean_DTR <- raw_temp_DTR_7 %>% 
  apply(., 2, apply.monthly, mean, na.rm = T) %>% 
  xts(., seq(as.Date("1981-01-01"), as.Date("2016-12-01"), by = "month")) %>% 
  apply(., 2, apply.yearly, mean, na.rm = T) %>% 
  apply(., 2, mean, na.rm = T) 

dt_tx <- raw_spat_St_eqc %>%
  data.frame(., mean_val = mean_TX)

dt_tn <- raw_spat_St_eqc %>%
  data.frame(., mean_val = mean_TN)

dt_dtr <- raw_spat_St_eqc %>%
  data.frame(., mean_val = mean_DTR)

################# tx

dat <-  dt_tx[,c("mean_val","ZZ_n","XX", "YY")]
D.geo <- dt_tx[,c("distsea")] %>% dist()
 
D0 <- dist(dat) # the socio-demographic distances
tree <- hclustgeo(D0)

plot(tree,hang=-1,label=FALSE, xlab="",sub="",
     main="Ward dendrogram with D0 only",cex.main=0.8,cex=0.8,cex.axis=0.8,cex.lab=0.8)

rect.hclust(tree,k=5,border=c(4,5,3,2,1))

D1 <- as.dist(D.geo)
range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0,D1,range.alpha,K,graph=TRUE)
plot(cr,norm=TRUE)

tree <- hclustgeo(D0,D1,alpha=0.5)
P5bis <- cutree(tree,3)


dt_tx$CLUST <- P5bis


ggplot() + 
  geom_point(data= dt_tx, aes(x = XX, y = YY, colour = factor(CLUST)),  size = 3) + 
  scale_color_manual("",values = c("black", "tomato1", "royalblue2","gray"), labels = c("TX & TN", "TX", "TN") ) + 
  # geom_point(data = df, aes(x = XX.x, y = YY.x), colour = "gray10", size = 3) + 
  # geom_point(data = df_tx, aes(x = XX, y = YY), colour = "orange", size = 3) +
  # geom_point(data = df_tn, aes(x = XX, y = YY), colour = "skyblue", size = 3) +
  coord_quickmap(xlim = c(-68.5, -81.35), ylim = c(-18.5, 0), expand = F) +  xlab("") + ylab("") + 
  theme(legend.position = c(-5, -5), 
        legend.background = element_rect(fill = NA))

################# tn

dat <-  dt_tn[,c("mean_val","XX", "YY","ZZ_n")]
D.geo <- dt_tn[,c("distsea")] %>% dist()

D0 <- dist(dat) # the socio-demographic distances
tree <- hclustgeo(D0)

plot(tree,hang=-1,label=FALSE, xlab="",sub="",
     main="Ward dendrogram with D0 only",cex.main=0.8,cex=0.8,cex.axis=0.8,cex.lab=0.8)

rect.hclust(tree,k=5,border=c(4,5,3,2,1))

D1 <- as.dist(D.geo)
range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0,D1,range.alpha,K,graph=TRUE)
plot(cr,norm=TRUE)

tree <- hclustgeo(D0,D1,alpha=0.5)
P5bis <- cutree(tree,3)


dt_tn$CLUST <- P5bis

ggplot() + 
  geom_point(data= dt_tn, aes(x = XX, y = YY, colour = factor(CLUST)),  size = 3) + 
  scale_color_manual("",values = c("black", "tomato1", "royalblue2","gray"), labels = c("TX & TN", "TX", "TN") ) + 
  # geom_point(data = df, aes(x = XX.x, y = YY.x), colour = "gray10", size = 3) + 
  # geom_point(data = df_tx, aes(x = XX, y = YY), colour = "orange", size = 3) +
  # geom_point(data = df_tn, aes(x = XX, y = YY), colour = "skyblue", size = 3) +
  coord_quickmap(xlim = c(-68.5, -81.35), ylim = c(-18.5, 0), expand = F) +  xlab("") + ylab("") + 
  theme(legend.position = c(-5, -5), 
        legend.background = element_rect(fill = NA))

################# tn

dat <-  dt_dtr[,c("mean_val","XX", "YY","ZZ_n")]
D.geo <- dt_dtr[,c("distsea")] %>% dist()

D0 <- dist(dat) # the socio-demographic distances
tree <- hclustgeo(D0)

plot(tree,hang=-1,label=FALSE, xlab="",sub="",
     main="Ward dendrogram with D0 only",cex.main=0.8,cex=0.8,cex.axis=0.8,cex.lab=0.8)

rect.hclust(tree,k=5,border=c(4,5,3,2,1))

D1 <- as.dist(D.geo)
range.alpha <- seq(0,1,0.1)
K <- 5
cr <- choicealpha(D0,D1,range.alpha,K,graph=TRUE)
plot(cr,norm=TRUE)

tree <- hclustgeo(D0,D1,alpha=0.5)
P5bis <- cutree(tree,3)


dt_dtr$CLUST <- P5bis

ggplot() + 
  geom_point(data= dt_dtr, aes(x = XX, y = YY, colour = factor(CLUST)),  size = 3) + 
  scale_color_manual("",values = c("black", "tomato1", "royalblue2","gray"), labels = c("TX & TN", "TX", "TN") ) + 
  # geom_point(data = df, aes(x = XX.x, y = YY.x), colour = "gray10", size = 3) + 
  # geom_point(data = df_tx, aes(x = XX, y = YY), colour = "orange", size = 3) +
  # geom_point(data = df_tn, aes(x = XX, y = YY), colour = "skyblue", size = 3) +
  coord_quickmap(xlim = c(-68.5, -81.35), ylim = c(-18.5, 0), expand = F) +  xlab("") + ylab("") + 
  theme(legend.position = c(-5, -5), 
        legend.background = element_rect(fill = NA))

#### 
#### 
all(dt_tx$CLUST == dt_tn$CLUST)#?

qc_stat <- data.frame(raw_spat_St_eqc[, c(1:7, 10)], GGR = dt_tx[, c(13)])
qc_stat$GGR[qc_stat$GGR == 2] <- 4
qc_stat$GGR[qc_stat$GGR == 3] <- 2
qc_stat$GGR[qc_stat$GGR == 4] <- 3

qc_tx <- raw_temp_TX_7
qc_tn <- raw_temp_TN_7
qc_dtr <- raw_temp_DTR_7


### saving data 

save(qc_stat, 
     qc_tx, qc_tn, qc_dtr,
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step05_QCDATA_05.RData"))

