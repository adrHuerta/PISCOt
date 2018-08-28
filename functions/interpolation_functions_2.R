get_variables <- function(MODIS_LST = NULL,  #brick
                          ALL_STATICS_VAR = NULL, #brick
                          OBS_DATA = NULL, #brick
                          m = NULL, #number
                          name_var = NULL) #character
  {
  
  #
  dmy_cov <- MODIS_LST[[paste("LST", m, sep = "")]]; names(dmy_cov) <- "LST"
  obs_data <- OBS_DATA[paste("To", m, sep = "")]; names(obs_data) <- "To"
  cov_data <- brick(dmy_cov, ALL_STATICS_VAR) %>% .[[name_var]]
  
  #
  formula_lm <- as.formula(paste("To ~ ",  paste(name_var, collapse = "+")))
  
  return(list(cov_data = cov_data, obs_data = obs_data, formula_lm = formula_lm))
}

# get_variables(MODIS_LST = tx_lst,
#               ALL_STATICS_VAR = rest_cov,
#               OBS_DATA = tx_obs_normal_1,
#               m = "12",
#               name_var = c("LST","Z"))

regKriging <- function(from_get_variables = NULL,
                       delR = NULL)
{
  
  #regression 
  
  cov_data <- from_get_variables$cov_data
  obs_data <- from_get_variables$obs_data
  formula_lm <- from_get_variables$formula_lm
  
  p_cov <- raster::extract(cov_data, obs_data, cellnumber = F, sp = T)
  lineral_model <- lm(formula_lm, p_cov)
  coeff_model <- summary(lineral_model)$coefficients[,1]
  obs_data$Residuals <- lineral_model$residuals
  
  if( is.numeric(delR)) {
    
    obs_data$Residuals[obs_data$Residuals > delR ] <- delR 
    obs_data$Residuals[obs_data$Residuals < -delR ] <- -delR 
    
  }
  
  cov_data_model <- coeff_model[1] + sum(cov_data*coeff_model[-1])
  #spplot(cov_data_model)
  
  #residual kriging 
  #.0 grid data
  
  point <- rasterToPoints(cov_data_model) %>% data.frame
  coordinates(point) <- ~x + y
  projection(point) <- NA
  
  #.1 variograms
  
  Residuals_df <- obs_data %>% as.data.frame()
  coordinates(Residuals_df) =~ XX+YY
  projection(obs_data) <- NA
  
  variogram = autofitVariogram(Residuals ~ 1, Residuals_df, 
                               fix.values = c(0, NA, NA),
                               cressie = T, miscFitOptions = list(merge.small.bins = T,min.np.bin = 30))
  
  #.2 kriging 
  
  residual_kriged <- krige(Residuals ~ 1, obs_data, point, model = variogram$var_model)
  residual_val <- as(residual_kriged[1],"SpatialPixelsDataFrame")
  gridded(residual_val) <- TRUE
  residual_val <- raster(residual_val)
  residual_var <- as(residual_kriged[2],"SpatialPixelsDataFrame")
  gridded(residual_var) <- TRUE
  residual_var <- raster(residual_var)
  
  # outputs 
  projection(residual_val) <- projection(cov_data_model)
  cov_data_model <- crop(cov_data_model, extent(residual_val))
  
  final_map <- residual_val + cov_data_model
  
  return(list(lineral_model = lineral_model, 
              cov_data_model = cov_data_model, 
              variogram = variogram,
              final_map = final_map,
              final_map_sd = residual_var))
}


get_variables2 <- function(CLIM_D = NULL,  #brick
                           ALL_STATICS_VAR = NULL, #brick
                           OBS_DATA = NULL, #brick
                           n = NULL, #number
                           name_var = NULL) #character
{
  
  #
  nM <- names(OBS_DATA)[n]
  obs_data <- OBS_DATA[,n]; names(obs_data) <- "To"
  dmy_cov <- CLIM_D[[ substr(nM, 9, 10) %>% as.numeric]]; names(dmy_cov) <- "CT"
  cov_data <- brick(dmy_cov, ALL_STATICS_VAR) %>% .[[name_var]]
  
  #
  formula_lm <- as.formula(paste("To ~ ",  paste(name_var, collapse = "+")))
  
  return(list(cov_data = cov_data, obs_data = obs_data, formula_lm = formula_lm, date = nM))
}

# get_variables2(CLIM_D = merging_tx,
#                ALL_STATICS_VAR = rest_cov,
#                OBS_DATA = anom_dtx,
#                n = 13149,
#                name_var = c("CT","X","Y","DS","TDI"))

regKriging2 <- function(from_get_variables = NULL,
                        delR = NULL)
{
  
  #regression 
  
  cov_data <- from_get_variables$cov_data
  obs_data <- from_get_variables$obs_data
  formula_lm <- from_get_variables$formula_lm
  
  p_cov <- raster::extract(cov_data, obs_data, cellnumber = F, sp = T)
  lineral_model <- lm(formula_lm, p_cov)
  coeff_model <- summary(lineral_model)$coefficients[,1]
  obs_data$Residuals <- lineral_model$residuals
  #delR can not be to low!
  if( is.numeric(delR)) {
    
    obs_data$Residuals[obs_data$Residuals > delR ] <- delR 
    obs_data$Residuals[obs_data$Residuals < -delR ] <- -delR 
    
  }
  
  cov_data_model <- coeff_model[1] + (cov_data*coeff_model[-1])#sum(cov_data*coeff_model[-1])
  #spplot(cov_data_model)
  
  #residual kriging 
  #.0 grid data
  
  point <- rasterToPoints(cov_data_model) %>% data.frame
  coordinates(point) <- ~x + y
  projection(point) <- NA
  
  #.1 variograms
  
  Residuals_df <- obs_data %>% as.data.frame()
  coordinates(Residuals_df) =~ XX+YY
  projection(obs_data) <- NA
  
  variogram = autofitVariogram(Residuals ~ 1, Residuals_df, 
                               fix.values = c(0, NA, NA),
                               cressie = T, miscFitOptions = list(merge.small.bins = T,min.np.bin = 30))
  
  
  #variogram too sensible!
  
  ################
  
  #.2 kriging 
  
  residual_kriged <- krige(Residuals ~ 1, obs_data, point, model = variogram$var_model)
  residual_val <- as(residual_kriged[1],"SpatialPixelsDataFrame")
  gridded(residual_val) <- TRUE
  residual_val <- raster(residual_val)
  residual_var <- as(residual_kriged[2],"SpatialPixelsDataFrame")
  gridded(residual_var) <- TRUE
  residual_var <- raster(residual_var)
  
  # outputs 
  projection(residual_val) <- projection(cov_data_model)
  cov_data_model <- crop(cov_data_model, extent(residual_val))
  
  #final_map <- (residual_val + cov_data_model)
  final_map <- from_get_variables$cov_data$CT + (residual_val + cov_data_model)

  return(list(lineral_model = lineral_model, 
              cov_data_model = cov_data_model, 
              variogram = variogram,
              final_map = final_map,
              final_map_sd = residual_var))
}


regKriging3 <- function(from_get_variables = NULL,
                       delR = NULL)
{
  
  #regression 
  
  cov_data <- from_get_variables$cov_data
  obs_data <- from_get_variables$obs_data
  formula_lm <- from_get_variables$formula_lm
  
  p_cov <- raster::extract(cov_data, obs_data, cellnumber = F, sp = T)
  lineral_model <- lm(formula_lm, p_cov)
  coeff_model <- summary(lineral_model)$coefficients[,1]
  obs_data$Residuals <- lineral_model$residuals
  #delR can not be to low!
  if( is.numeric(delR)) {
    
    obs_data$Residuals[obs_data$Residuals > delR ] <- delR 
    obs_data$Residuals[obs_data$Residuals < -delR ] <- -delR 
    
  }
  
  cov_data_model <- coeff_model[1] + (cov_data*coeff_model[-1])#sum(cov_data*coeff_model[-1])
  #spplot(cov_data_model)
  
  #residual kriging 
  #.0 grid data
  
  point <- rasterToPoints(cov_data_model) %>% data.frame
  coordinates(point) <- ~x + y
  projection(point) <- NA
  
  #.1 variograms
  
  Residuals_df <- obs_data %>% as.data.frame()
  coordinates(Residuals_df) =~ XX+YY
  projection(obs_data) <- NA
  
  # variogram = autofitVariogram(Residuals ~ 1, Residuals_df,
  #                              fix.values = c(0, NA, NA),
  #                              cressie = T, miscFitOptions = list(merge.small.bins = T,min.np.bin = 30))
  # variogram = autofitVariogram(Residuals ~ 1, Residuals_df, 
  #                             fix.values = c(0, NA, NA))
  # variogram <- tryCatch(autofitVariogram(Residuals ~ 1, Residuals_df,
  #                                        fix.values = c(NA, NA, NA),
  #                                        cressie = T, miscFitOptions = list(merge.small.bins = T,min.np.bin = 30)), 
  #                       error = function(e) {
  #                         
  #                         tryCatch(autofitVariogram(Residuals ~ 1, Residuals_df, fix.values = c(NA, NA, NA), cressie = T), 
  #                                  error = function(e) {autofitVariogram(Residuals ~ 1, Residuals_df, fix.values = c(0, NA, NA))})
  #                         
  #                       })
  # 
  #variogram too sensible!
  
  
  ################33
  
  Residuals_adr <- as.data.frame(Residuals_df, xy = T)
  tps <- fields::Tps(Residuals_adr[,c("XX","YY")], Residuals_adr[,"Residuals"])
  p <- raster(cov_data_model)
  p <- interpolate(p, tps) #%>%  mask(., cov_data_model)
  
  ################
  
  #.2 kriging 
  
  # residual_kriged <- krige(Residuals ~ 1, obs_data, point, model = variogram$var_model)
  # residual_val <- as(residual_kriged[1],"SpatialPixelsDataFrame")
  # gridded(residual_val) <- TRUE
  # residual_val <- raster(residual_val)
  # residual_var <- as(residual_kriged[2],"SpatialPixelsDataFrame")
  # gridded(residual_var) <- TRUE
  # residual_var <- raster(residual_var)
  
  #conditional
  # if(all(is.na(residual_var@data@values)) | any(residual_val@data@values[!is.na(residual_val@data@values)] > (delR + 0.5))){
  #   
  #   idwError <- idw(Residuals ~ 1, obs_data, point, idp = 2)
  #   idwError <- idwError["var1.pred"]
  #   gridded(idwError) <- TRUE
  #   residual_val <- raster(idwError)
  #   residual_var <- NA
  #   variogram <- NA
  #   }
  
  # outputs 
  #projection(residual_val) <- projection(cov_data_model)
 # cov_data_model <- crop(cov_data_model, extent(residual_val))
  
  #final_map <- (residual_val + cov_data_model)
  #final_map <- from_get_variables$cov_data$CT + (residual_val + cov_data_model)
  final_map <- from_get_variables$cov_data$CT + (p + cov_data_model)
  
  return(list(lineral_model = lineral_model, 
              cov_data_model = cov_data_model, 
             # variogram = variogram,
              final_map = final_map)
         #     final_map_sd = residual_var)
         )
}

