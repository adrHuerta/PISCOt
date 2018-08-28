rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(ggplot2)
library(ggrepel)
library(sp)
library(maptools)
library(raster)
library(gstat)

### source codes 

source('./functions/interpolation_functions.R')

###

  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","anom_obs_dataset.RData"))
  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_tn_RASTERS_R.RData"))
  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","monthly_tx_RASTERS_R.RData"))
  load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))
  
  # raster_dailyvalues_output <- file.path("/media","buntu","D1AB-BCDE","databases","results","daily_values","data")
  # raster_output <- file.path("/media","buntu","D1AB-BCDE","databases","results","monthly_normals","data")
  ls()

#######
  
plot(anom_dtx[,186]@data %>% unlist(), extract(merging_tx[[1]], anom_dtx[,1]))
plot(anom_dtx[,186]@data %>% unlist(), extract(rest_cov[[1]], anom_dtx[,1]))
plot(anom_dtx[,186]@data %>% unlist(), extract(rest_cov[[2]], anom_dtx[,1]))
plot(anom_dtx[,186]@data %>% unlist(), extract(merging_tx[[3]], anom_dtx[,1]))
plot(anom_dtx[,186]@data %>% unlist(), extract(merging_tx[[4]], anom_dtx[,1]))
plot(anom_dtx[,186]@data %>% unlist(), extract(merging_tx[[5]], anom_dtx[,1]))

rest_cov <- crop(rest_cov, extent(merging_tx[[2]]))

ress <- data.frame(AT = anom_dtx[,186]@data %>% unlist(),
                   CT = extract(merging_tx[[6]], anom_dtx[,1]), 
                 #  Z = extract(rest_cov[[1]], anom_dtx[,1]),
                   X =extract(rest_cov[[2]], anom_dtx[,1]),
                   Y = extract(rest_cov[[3]], anom_dtx[,1]),
                   DS = extract(rest_cov[[4]], anom_dtx[,1]),
                   TDI = extract(rest_cov[[5]], anom_dtx[,1]))

cov_data <- brick(merging_tx[[2]], rest_cov[[2]],
                  rest_cov[[3]], rest_cov[[4]], rest_cov[[5]])
lineral_model <- MASS::rlm(AT ~ CT + X + Y + DS + TDI, ress) 
plot(lineral_model)
coeff_model <- summary(lineral_model)$coefficients[,1]

 obs_data <- anom_dtx[,1]
 obs_data$Residuals <- lineral_model$residuals
 obs_data$Residuals[obs_data$Residuals > 3 ] <- 2.5
 obs_data$Residuals[obs_data$Residuals < -3 ] <- -2.5 
 
 
 cov_data_model <- coeff_model[1] + sum(cov_data*coeff_model[-1])
 spplot(cov_data_model)
 
 point <- rasterToPoints(cov_data_model) %>% data.frame
 coordinates(point) <- ~x + y
 projection(point) <- projection(cov_data_model)
 projection(point) <- NA
 
 #
 Residuals_df <- obs_data %>% as.data.frame()
 coordinates(Residuals_df) =~ XX+YY
 projection(obs_data) <- NA
 
 
 #### 
 
 variogram = autofitVariogram(Residuals ~ 1, Residuals_df, 
                              fix.values = c(NA, NA, NA),
                              cressie = T, miscFitOptions = list(merge.small.bins = T,min.np.bin = 30))
 plot(variogram)
 
 
 residual_kriged <- krige(Residuals ~ 1, obs_data, point, model = variogram$var_model)
 
 #####
 
 library(fields)
 m <- Tps(coordinates(obs_data), obs_data$Residuals)
 tps <- interpolate(cov_data_model, m)
 tps <- mask(tps, idw)
 spplot(tps)
 spplot(cov_data_model + tps)
 spplot(cov_data_model + tps + merging_tn[[2]])
 uncertaintyError
 ####
 
 idpR = seq(0.8, 3.5, 0.1)
 idpRange <- idpR
 mse <- rep(NA, length(idpRange))
 for (i in 1:length(idpRange)) {
   mse[i] <- mean(krige.cv(Residuals ~ 1, Residuals_df, nfold = nrow(Residuals_df), set = list(idp = idpRange[i]), verbose = F)$residual^2)
 }
 poss <- which(mse %in% min(mse))
 bestparam <- idpRange[poss]
 residual.best <- krige.cv(Residuals ~ 1, Residuals_df, nfold = nrow(Residuals_df), set = list(idp = idpRange[poss]), verbose = F)$residual
 
 
 idwError <- idw(Residuals ~ 1, obs_data, point, idp = bestparam)
 idwError <- idwError["var1.pred"]
 gridded(idwError) <- TRUE
 mapa <- raster(idwError)
 spplot(cov_data_model + mapa + merging_tn[[2]])
 
 colfunc <- colorRampPalette(c("white","#3398efff", "#06cdd2ff","#b7e52dff","#ffb42bff","#ff5454ff"))
 (cov_data_model + mapa + merging_tx[[2]]) %>%
   as.data.frame(xy = T) %>% 
   ggplot() + 
   geom_tile(aes(x = x, y = y, fill = layer)) + 
   scale_fill_gradientn("",colors = colfunc(20))  
 
 ######
 
 residual_val <- as(residual_kriged[1],"SpatialPixelsDataFrame")
 gridded(residual_val) <- TRUE
 residual_val <- raster(residual_val)
 residual_var <- as(residual_kriged[2],"SpatialPixelsDataFrame")
 gridded(residual_var) <- TRUE
 residual_var <- raster(residual_var)
 
 spplot(cov_data_model + residual_val)
 
 spplot(cov_data_model + residual_val + merging_tn[[2]])
 plot(cov_data_model + residual_val + merging_tn[[2]])
 rasterVis::levelplot(cov_data_model + residual_val + merging_tn[[2]])

 colfunc <- colorRampPalette(c("white","#3398efff", "#06cdd2ff","#b7e52dff","#ffb42bff","#ff5454ff"))
 (cov_data_model + residual_val + merging_tx[[2]]) %>%
   as.data.frame(xy = T) %>% 
   ggplot() + 
   geom_tile(aes(x = x, y = y, fill = layer)) + 
   scale_fill_gradientn("",colors = colfunc(20))  
   
 #######

normal_tn <- file.path(raster_output, "tn") %>% 
  list.files(., full.names = T) %>%
  as.list() %>% 
  brick()
projection(normal_tn) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


########
########

daily_variability <- names(tn_obs_daily) %>% 
  .[-length(.)] %>%
  data.frame(daily = ., normal = paste("normal_tn_", substr(., 9, 10), sep = ""), 
             stringsAsFactors = F)


########
########

daily_tn_results <- list()

#for(j in 1411:dim(daily_variability)[1] ){
foreach(j=1:12783) %dopar% {  
  formula_tn <- c(daily_variability[j, 1], daily_variability[j, 2])
  
  daily_tn_results[[j]]  <- RIDW(gauge = tn_obs_daily,
                                 cov = normal_tn,
                                 formula = formula_tn)
  
  raster_names <- file.path(raster_dailyvalues_output, "tn") %>%
    file.path(., paste(daily_variability[j, 1], ".tif", sep = ""))
  
  writeRaster(daily_tn_results[[j]]$Interpol,
              raster_names,
              format = "GTiff",
              overwrite = F)
}