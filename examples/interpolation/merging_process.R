rest_cov <- brick(dem_data_resample,Xmap, Ymap, dsea_data_resample,res_TDI_resample)
names(rest_cov) <- c("Z","X","Y","DS","TDI")
names(tn_lst) <- paste("LST", 1:12, sep = "")
tn_obs_normal_1 <- tn_obs_normal[,-13]
names(tn_obs_normal_1) <- paste("To", 1:12, sep = "")

#
require(foreign)
require(MASS)
require(automap)

dmy = "12"
var_lm = c("LST","Z")

#

dmy_cov <- tx_lst[[paste("LST", dmy, sep = "")]]; names(dmy_cov) <- "LST"

obs_data <- tx_obs_normal_1[paste("To", dmy, sep = "")]; names(obs_data) <- "To"
cov_data <- brick(dmy_cov, rest_cov) %>% .[[var_lm]]

#que por mecesidad de servicio y se tiene que generar la informac gril de temṕ ya que sera insumo de modl a tiempo real 
#


formula_lm <- as.formula(paste("To ~ ",  paste(var_lm, collapse = "+")))

p_cov <- raster::extract(cov_data, obs_data, cellnumber = F, sp = T)
lineral_model <- rlm(formula_lm, p_cov)
coeff_model <- summary(lineral_model)$coefficients[,1]
obs_data$Residuals <- lineral_model$residuals
obs_data$Residuals[obs_data$Residuals > 3 ] <- 3 
obs_data$Residuals[obs_data$Residuals < -3 ] <- -3 

#

cov_data_model <- coeff_model[1] + sum(cov_data*coeff_model[-1])
spplot(cov_data_model)

#grid
point <- rasterToPoints(cov_data_model) %>% data.frame
coordinates(point) <- ~x + y
projection(point) <- projection(cov_data_model)
projection(point) <- NA

#
Residuals_df <- obs_data %>% as.data.frame()
coordinates(Residuals_df) =~ XX+YY
projection(obs_data) <- NA

variogram = autofitVariogram(Residuals ~ 1, Residuals_df, 
                             fix.values = c(NA, NA, NA),
                            cressie = T, miscFitOptions = list(merge.small.bins = T,min.np.bin = 30))
plot(variogram)

residual_kriged <- krige(Residuals ~ 1, obs_data, point, model = variogram$var_model)
residual_val <- as(residual_kriged[1],"SpatialPixelsDataFrame")
gridded(residual_val) <- TRUE
residual_val <- raster(residual_val)
residual_var <- as(residual_kriged[2],"SpatialPixelsDataFrame")
gridded(residual_var) <- TRUE
residual_var <- raster(residual_var)

spplot(cov_data_model + residual_val)
#spplot(residual_var, at = c(0:10))
