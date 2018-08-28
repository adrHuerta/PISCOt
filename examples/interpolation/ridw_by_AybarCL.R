library(raster)
library(gstat)
data("Dorado", package = "Dorado")
gauge <- Dorado::mean_doble_Station(gauge = Dorado$gauge,cov = Dorado$TRMM)
cov <- Dorado$TRMM
formula = PP_anual~Precipitacion_Anual

ext <- raster::extract(cov, gauge, cellnumber = F, sp = T)
station <- gauge

linear <- na.omit(ext@data) %>% tbl_df %>% mutate_all(as.character) %>%
  mutate_all(as.numeric)

llm <- lm(formula,linear)
station$residuals <- llm$residuals
#linear$PP_anual - predict(llm, linear) 

point <- rasterToPoints(cov) %>% data.frame
coordinates(point) <- ~x + y
projection(point) <- projection(cov)
plot(point)

idpR = seq(0.8, 3.5, 0.1)
idpRange <- idpR
mse <- rep(NA, length(idpRange))
for (i in 1:length(idpRange)) {
  mse[i] <- mean(krige.cv(residuals ~ 1, station, nfold = nrow(station), set = list(idp = idpRange[i]), verbose = F)$residual^2)
}
poss <- which(mse %in% min(mse))
bestparam <- idpRange[poss]
residual.best <- krige.cv(residuals ~ 1, station, nfold = nrow(station), set = list(idp = idpRange[poss]), verbose = F)$residual


idwError <- idw(residuals ~ 1, station, point, idp = bestparam)
idwError <- idwError["var1.pred"]
gridded(idwError) <- TRUE
mapa <- raster(idwError)

namesF <- unlist(strsplit(as.character(formula), " "))
max_k <- floor(length(namesF)/2) + 1
name_cov = namesF[!namesF %in% c("~", "+", "-", "*", "/")][2:max_k]
cov <- cov[[name_cov]]

OBSp <- sum(stack(mapply(function(i) cov[[i]] * llm$coefficients[i + 1],
                         1:nlayers(cov)))) + llm$coefficients[1]

Ridw <- OBSp + mapa
