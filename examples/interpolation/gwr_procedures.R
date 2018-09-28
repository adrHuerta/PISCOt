rm(list = ls())

library(dplyr)
library(data.table)
library(xts)
library(automap)
library(sp)
library(maptools)
library(raster)
library(gstat)
library(MASS)
### source codes 

source('/media/buntu/D1AB-BCDE/github_repos/PISCOt/functions/interpolation_functions_2.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","normal_obs_dataset.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","spatial_predictors.RData"))
raster_output <- file.path("/media","buntu","D1AB-BCDE","databases","results","monthly_normals","data")
ls()

#standardized cov 
## observed 
# tn_obs_normal_std <- tn_obs_normal
# tx_obs_normal_std <- tx_obs_normal
# 
# tn_obs_normal_std@data[, 1:12] <- apply(tn_obs_normal_std@data[, 1:12], 2, function(z) (z-mean(z))/sd(z))
# colnames(tx_obs_normal_std@data) <- colnames(tn_obs_normal_std@data) 
# colnames(tx_obs_normal@data) <- colnames(tn_obs_normal_std@data) 
# tx_obs_normal_std@data[, 1:12] <- apply(tx_obs_normal_std@data[, 1:12], 2, function(z) (z-mean(z))/sd(z))
# 
# ## cov 

# rest_cov_std <- lapply(rest_cov %>% as.list(), function(z){
#   value_p <- extract(z, tx_obs_normal)
#   (z - mean(value_p))/sd(value_p)
#   }) %>% brick()
# 
# tn_lst_std <- lapply(tx_lst %>% as.list(), function(z){
#   value_p <- extract(z, tn_obs_normal)
#   (z - mean(value_p))/sd(value_p)
# }) %>% brick()
# 
# tn_lst_std <- lapply(tx_lst %>% as.list(), function(z){
#   value_p <- extract(z, tn_obs_normal)
#   (z - mean(value_p))/sd(value_p)
# }) %>% brick()

shp_peru <- readOGR("/home/buntu/Desktop/prueba/drought_analysis/shape_dist/depart.shp","depart")

#tn
j = 4
from_get_variables <-  get_variables(MODIS_LST = tx_lst,
                           ALL_STATICS_VAR = rest_cov,
                           OBS_DATA = tx_obs_normal,
                           m = j %>% as.character(),
                           name_var = c("LST","Z","X","Y"))


cov_data <- from_get_variables$cov_data
obs_data <- from_get_variables$obs_data
formula_lm <- from_get_variables$formula_lm

p_cov <- raster::extract(cov_data, obs_data, cellnumber = F, sp = T)

bw_50 <- GWmodel::bw.gwr(formula_lm, data = p_cov, approach = "AICc", kernel = "gaussian", adaptive = TRUE)
gw_ss_bs_50 <- GWmodel::gwss(p_cov, vars  =  c("To", "LST", "Z", "X", "Y"),
                             kernel = "tricube", adaptive = TRUE, bw = bw_50, quantile = TRUE)

bw_all <- 178
gw_ss_bs_all <- GWmodel::gwss(p_cov, vars  =  c("To", "LST", "Z", "X", "Y"),
                              kernel = "boxcar", adaptive = TRUE, bw = bw_all, quantile = TRUE)


colfunc02 <- colorRampPalette(c("#3398efff","#06cdd2ff","#b7e52dff","white","#ffb42bff","#ff5454ff","black"))

df_50 <- gw_ss_bs_50$SDF@data %>% 
  .[1:164,] %>%
  .[,c(c("Corr_To.LST","Corr_To.Z","Corr_To.X","Corr_To.Y"))] %>%
  data.frame(gw_ss_bs_50$SDF@coords[1:164,], .) %>% 
  tidyr::gather(key, value, -XX, -YY) %>%
  data.frame(., bw = "LOCAL")

df_all <- gw_ss_bs_all$SDF@data %>%
  .[1:164,] %>%
  .[,c(c("Corr_To.LST","Corr_To.Z","Corr_To.X","Corr_To.Y"))] %>%
  data.frame(gw_ss_bs_50$SDF@coords[1:164,], .) %>% 
  tidyr::gather(key, value, -XX, -YY)  %>%
  data.frame(., bw = "GLOBAL")

rbind(df_50, df_all) %>%
  mutate(value_cut = cut(value, breaks = seq(-1,1, .1), include.lowest = T)) %>%
  ggplot() + 
  geom_polygon(data = shp_peru, aes(y = lat, x = long, group=group), fill = "NA", colour = "gray10", size = 0.45) + 
  geom_point(aes(x = XX, y = YY, fill = value_cut), shape = 21, size = 5) + 
  scale_fill_manual("r",values = rev(colfunc02(20)), drop = F) + 
  facet_grid(bw~key) + xlab("") + ylab("") + theme_bw()

# Calculate GW-ss for global model and then with decreasing bandwidths

bandwidth <- 178
gw_ss_global <- GWmodel::gwss(p_cov, vars  =  c("To", "LST", "Z", "X", "Y", "DS"),
                     kernel = "bisquare", adaptive = TRUE, bw = bandwidth, quantile = TRUE)
bandwidth <- 150
gw_ss_bs_150 <- GWmodel::gwss(p_cov, vars  =  c("To", "LST", "Z", "X", "Y", "DS"),
                     kernel = "bisquare", adaptive = TRUE, bw = bandwidth, quantile = TRUE)
bandwidth <- 100
gw_ss_bs_100 <- GWmodel::gwss(p_cov, vars  =  c("To", "LST", "Z", "X", "Y", "DS"),
                     kernel = "bisquare", adaptive = TRUE, bw = bandwidth, quantile = TRUE)
bandwidth <- 50
gw_ss_bs_50 <- GWmodel::gwss(p_cov, vars  =  c("To", "LST", "Z", "X", "Y", "DS"),
                    kernel = "bisquare", adaptive = TRUE, bw = bandwidth, quantile = TRUE)




lm.global <- lm(To ~ LST + Z + X + Y + DS, data = p_cov)
fmsb::VIF(lm.global)

BKWcn <- function(X) {
   p <- dim(X)[2]
   Xscale <- sweep(X, 2, sqrt(colSums(X^2)), "/")
   Xsvd <- svd(Xscale)$d
   Xsvd[1] / Xsvd[p]
   }
X <- as.matrix(cbind(1,p_cov@data[1:2]))
BKWcn(X)

#########

nobs <- dim(p_cov)[1]
lcrm1 <- GWmodel::gwr.lcr(To ~ LST + Z + X + Y, data = p_cov, bw = bw_50, kernel = "bisquare", 
                          adaptive=TRUE,lambda = T,  regression.points = newpts)
glcr_adr(To ~ LST + Z + X + Y, data = p_cov, bw = bw_50, kernel = "bisquare", 
         adaptive=TRUE,lambda = T,  regression.points = newpts)
lcrm1 <- adr5(To ~ LST + Z + X + Y, data = p_cov, bw = lcrm2.bw, kernel = "bisquare", 
                          adaptive=TRUE,lambda = T,  regression.points = newpts)

lcrm2.bw <- GWmodel::bw.gwr.lcr(To ~ LST + Z + X + Y, data = p_cov, kernel = "bisquare", adaptive=TRUE)


lcrm2 <- GWmodel::gwr.lcr(formula = formula_lm, data = p_cov, bw = 100, kernel = "bisquare", adaptive = TRUE)
spplot(lcrm2$SDF[,5])

gwr.cv.bw <- GWmodel::bw.gwr(formula_lm, data = p_cov, approach = "CV", kernel = "bisquare", adaptive = TRUE)

### basic GWR 

bw <- GWmodel::bw.gwr(formula_lm, data = p_cov, approach = "AICc", kernel = "tricube", adaptive = TRUE)
newpts <- rasterToPoints(raster(cov_data))
gwr_model <- GWmodel::gwr.basic(formula_lm, data = p_cov, bw = bw,  kernel = "tricube", adaptive = TRUE,  regression.points = newpts)
# gwr_model2 <- gwrr::gwl.est(form = formula_lm, locs = newpts[, 1:2], data = p_cov@data, kernel = "gauss")

#GWR coeff

coef_GWR <- cov_data %>% mask(., cov_data)
coef_GWR@data@values[, names(cov_data)] <- gwr_model$SDF@data[, names(cov_data)] %>% data.matrix()
coef_GWR <- coef_GWR %>% mask(., cov_data[[1]])

coef_GWR_intercept <- cov_data[[1]]
names(coef_GWR_intercept) <- "Int"
coef_GWR_intercept@data@values <-  gwr_model$SDF$`Intercept`
coef_GWR_intercept <- coef_GWR_intercept %>% mask(., cov_data[[1]])

cov_data_model <- coef_GWR_intercept + sum(coef_GWR*cov_data)

#GWR residuals
obs_data <- raster::extract(cov_data_model, obs_data, cellnumber = F, sp = T)
obs_data$Residuals <- obs_data$To - obs_data$layer

