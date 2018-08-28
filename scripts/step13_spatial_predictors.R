rm(list = ls())

library(dplyr)
library(sp)
library(maptools)
library(raster)
library(gstat)
library(rgdal)

### source codes 

source('./functions/tools_get_os.R')
source('./functions/interpolation_functions.R')

######## gridded zone #########

zone_gridd <- readOGR("/media/buntu/D1AB-BCDE/My_inspiration/clima_2018/zone_poly.shp")
zone_gridd <- raster(extent(zone_gridd), resolution = 0.1)
projection(zone_gridd) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

################ DEM dataset ########################

dem_data <- file.path("/media", "buntu", "D1AB-BCDE", "databases", "spatial_predictors","dem_and_others3","GMTED2010_gee.tif") %>% #dem.tif
  raster()
#  crop(., extent(zone_gridd)) %>%
#  resample(., zone_gridd)
#writeRaster(dem_data, "dem_data.tif")
dem_data_cut <- dem_data %>% crop(., extent(zone_gridd))
dem_data_resample <- dem_data %>%  resample(., zone_gridd)

plot(dem_data)
plot(dem_data_cut)
dem_data_resample[dem_data_resample < 0] <- 0
plot(dem_data_resample)
################ DIST SEA ########################

dsea_data <- file.path("/media", "buntu", "D1AB-BCDE", "databases", "spatial_predictors","dem_and_others3","Distance_OceanPA_rz_2.tif") %>%
  raster() 
dsea_data <- projectRaster(dsea_data, crs = projection(zone_gridd)  ,method="ngb")
dsea_data_cut <- dsea_data %>% crop(., extent(zone_gridd)) 
dsea_data_resample <- dsea_data_cut %>% resample(., zone_gridd)
plot(dsea_data)
plot(dsea_data_cut)
plot(dsea_data_resample)

################# X #######################

Xmap <- dem_data_resample
Xmap[,] <- coordinates(dem_data_resample)[,1]
plot(Xmap)

################# Y #######################

Ymap <- dem_data_resample
Ymap[,] <- coordinates(dem_data_resample)[,2]
plot(Ymap)


############### MODIS LST ##################

# lst_night_raw <- brick("/home/buntu/Downloads/lst_night_raw.nc")   %>%
#   crop(., extent(zone_gridd)) %>%
#   resample(., zone_gridd)
# lst_night_raw <- lst_night_raw-273.5
# 
# lst_day_raw <- brick("/home/buntu/Downloads/lst_day_raw.nc")   %>%
#   crop(., extent(zone_gridd)) %>%
#   resample(., zone_gridd)
# lst_day_raw <- lst_day_raw-273.5
# plot(brick("/media/buntu/D1AB-BCDE/databases/spatial_predictors/modis_lst/monthly/tx/01.tif"))
# plot(brick("/media/buntu/D1AB-BCDE/databases/spatial_predictors/modis_lst/monthly/tn/02.tif"))
# tx_lst <- file.path("/media","buntu","D1AB-BCDE","databases","spatial_predictors","modis_lst","monthly","tx")
# tn_lst <- file.path("/media","buntu","D1AB-BCDE","databases","spatial_predictors","modis_lst","monthly","tn")

################ LST MODIS dataset ################

tx_lst <- brick("/media/buntu/D1AB-BCDE/databases/spatial_predictors/modis_lst2/raw_data/lst_day.nc")

projection(tx_lst) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#names(tx_lst) <- paste("TX_lst_", formatC(1:12, 1, flag = 0), sep = "")

tn_lst <-  brick("/media/buntu/D1AB-BCDE/databases/spatial_predictors/modis_lst2/raw_data/lst_night.nc")

projection(tn_lst) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#names(tn_lst) <- paste("TN_lst_", formatC(1:12, 1, flag = 0), sep = "")

## resampling to 5km.

# lst_5km <- lst_pentadal %>%
#   list.files(., full.names = T) %>% 
#   .[1] %>%
#   raster()
# projection(lst_5km) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

tx_lst <- resample(tx_lst, zone_gridd) - 273.15
names(tx_lst) <- paste("LST", 1:12, sep = "")
tn_lst <- resample(tn_lst, zone_gridd)  - 273.15
names(tn_lst) <- paste("LST", 1:12, sep = "")

##### TDI ###############

elev = dem_data

### 10km

TDI_10km <- focal(elev , 
                  w = matrix(1, nrow = 5, ncol = 5), 
                  fun = function(x){
                    #TDI
                    (x[13] - min(x))/(max(x)-min(x)) 
                  })

### 15km

TDI_15km <- focal(elev , 
                  w = matrix(1, nrow = 7, ncol = 7), 
                  fun = function(x){
                    #TDI
                    (x[25] - min(x))/(max(x)-min(x)) 
                  })

### 20km

TDI_20km <- focal(elev , 
                  w = matrix(1, nrow = 9, ncol = 9), 
                  fun = function(x){
                    #TDI
                    (x[41] - min(x))/(max(x)-min(x)) 
                  })

### 25km

TDI_25km <- focal(elev , 
                  w = matrix(1, nrow = 11, ncol = 11), 
                  fun = function(x){
                    #TDI
                    (x[61] - min(x))/(max(x)-min(x)) 
                  })

### 30km

TDI_30km <- focal(elev , 
                  w = matrix(1, nrow = 13, ncol = 13), 
                  fun = function(x){
                    #TDI
                    (x[85] - min(x))/(max(x)-min(x)) 
                  })

res_TDI <- TDI_10km + TDI_15km + TDI_20km + TDI_25km + TDI_30km

res_TDI <- crop(res_TDI, extent(zone_gridd))
res_TDI_resample <- resample(res_TDI, zone_gridd)
#writeRaster(res_TDI_resample, "/media/buntu/D1AB-BCDE/databases/spatial_predictors/res_TDI_resample.nc")
res_TDI_resample <- raster("/media/buntu/D1AB-BCDE/databases/spatial_predictors/res_TDI_resample_filled.nc") %>%
  mask(., tn_lst[[1]])

################## plot 

# shp_peru <- readOGR("/home/buntu/Desktop/serrano_temperature_dataset/Data_Perú_Geografico/Departamentos.shp", "Departamentos")
# projection(shp_peru) <- projection(res_TDI)
# 
# 
# 
# library(gridExtra)
# library(grid)
# library(rasterVis)
# 
# colfunc <- colorRampPalette(c("purple", "skyblue", "cyan", "yellow", "red"))
# 
# p1 <- levelplot(tx_lst[[1]], col.regions = colfunc(300), at = seq(0, 35, .25), 
#                 xlab = "", ylab = "", margin  = F, par.settings = list(axis.line = list(col = "transparent"), 
#                                                                        strip.background = list(col = 'transparent'), 
#                                                                        strip.border = list(col = 'transparent')), 
#                 scales = list(draw = FALSE), 
#                 colorkey=list(space="left"),
#                 xlim = c(-79, -69), ylim = c(-17, -5)) +
#   layer(sp.polygons(shp_peru, fill = NA, col = "gray10", lwd = 1.2))
# 
# p2 <- levelplot(tn_lst[[1]], col.regions = colfunc(300), at = seq(-10, 30, .25), 
#                 xlab = "", ylab = "", margin  = F, par.settings = list(axis.line = list(col = "transparent"), 
#                                                                        strip.background = list(col = 'transparent'), 
#                                                                        strip.border = list(col = 'transparent')), 
#                 scales = list(draw = FALSE), 
#                 colorkey=list(space="left"),
#                 xlim = c(-79, -69), ylim = c(-17, -5)) +
#   layer(sp.polygons(shp_peru, fill = NA, col = "gray10", lwd = 1.2))
# 
# 
# elev2 = elev
# elev2[elev2 < 1] = NA
# p3 <- levelplot(elev2, col.regions = terrain.colors(50, alpha = 1), at = seq(0, 6000, 500), 
#                 xlab = "", ylab = "", margin  = F, par.settings = list(axis.line = list(col = "transparent"), 
#                                                                        strip.background = list(col = 'transparent'), 
#                                                                        strip.border = list(col = 'transparent')), 
#                 scales = list(draw = FALSE), 
#                 colorkey=list(space="left"),
#                 xlim = c(-79, -69), ylim = c(-17, -5)) +
#   layer(sp.polygons(shp_peru, fill = NA, col = "gray10", lwd = 1.2))
# 
# 
#  p4 <- levelplot(res_TDI, col.regions = terrain.colors(50, alpha = 1), at = seq(0, 5, 0.1), 
#                 xlab = "", ylab = "", margin  = F, par.settings = list(axis.line = list(col = "transparent"), 
#                                                                        strip.background = list(col = 'transparent'), 
#                                                                        strip.border = list(col = 'transparent')), 
#                 scales = list(draw = FALSE), 
#                 colorkey=list(space="left"),
#                 xlim = c(-79, -69), ylim = c(-17, -5)) +
#   layer(sp.polygons(shp_peru, fill = NA, col = "gray10", lwd = 1.2))
# 
# 
#  grid.arrange(p3, p4, p1, p2, ncol=2)


### saving data 
rest_cov <- brick(dem_data_resample,Xmap, Ymap, dsea_data_resample,res_TDI_resample)
names(rest_cov) <- c("Z","X","Y","DS","TDI")

  save(rest_cov,
       tx_lst, tn_lst, 
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))

  