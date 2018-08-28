## interpolation functions

check_NA_pixels <- function(Rr, spt_point)
  {
  if( !(projection(Rr) == projection(spt_point)) ){
    sprintf("There is not an identical projection")
  } 
  
   res_v <- extract(Rr, spt_point, cellnumber = T) %>% 
    data.frame(., data.frame(spt_point)) %>% .[, -c(1, ncol(.))]
   colnames(res_v) <- c("value","CC", "XX", "YY")
   
   Tpixel <- res_v[complete.cases(res_v),] %>% .[, -1]
   TNApixel <- res_v[!complete.cases(res_v),] %>% .[, -1]
   
   return(list(match = Tpixel, 
               NAmatch = TNApixel) )
  
  }

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

changing_pixel_location <- function(Rr, spt_point_NA)
 {

 points_NA <- split(spt_point_NA, seq(nrow(spt_point_NA)))
 
 for(i in 1:length(points_NA)){
   
   points_sp <- points_NA[[i]]
   
   ext_p <- extent( c(points_sp$XX - 0.1, points_sp$XX + 0.1, 
                      points_sp$YY - 0.1, points_sp$YY + 0.1) )
   
   crop_p <- crop(Rr, ext_p)
   
   coordinates(points_sp) <- ~XX+YY
   projection(points_sp) <- projection(Rr)
   
   df_dist <- distanceFromPoints(crop_p, points_sp) %>% rasterToPoints()
   df_vales <- crop_p %>% rasterToPoints() 
   
   res <- merge(df_dist, df_vales, by = c("x", "y")) %>%
     .[order(.$layer),] %>% .[1, ]
   
   
   points_NA[[i]]$XX <- specify_decimal(res$x, 3) %>% as.numeric()
   points_NA[[i]]$YY <- specify_decimal(res$y, 3) %>% as.numeric()
   
   }
  
 return(do.call("rbind", points_NA))

}


mclim_dts <- function(nclim_values,
                      ts_t = seq(as.Date("1981-01-01"), as.Date("2016-12-31"), by = "day")){
  
  
  time_serie <- zoo(rep(NA, length(ts_t)), 
                    ts_t)
  for(i in 1:12)
  {
    time_serie[as.numeric(format(time(time_serie), "%m")) %in% i] <- nclim_values[i]
  }
  return(time_serie)
}


#RIDW original function made by AybarCL [https://github.com/AybarCL/] 
RIDW <- function(gauge, cov, formula, idpR = seq(0.8, 3.5, 0.1),...) {
  
  cov <- cov[[ formula[2] ]]
  gauge <- gauge[ formula[1] ]
  
  formula <- formula(paste(formula[1], "~", formula[2]))
  
  ext <- raster::extract(cov, gauge, cellnumber = F, sp = T)
  station <- gauge
  linear <- na.omit(ext@data) %>% tbl_df %>% mutate_all(as.character) %>%
    mutate_all(as.numeric)
  llm <- lm(formula,linear)
  station$residuals <- llm$residuals
  
  ####
  
  ext@data %>% 
    plot()
  
  # Define Grid -------------------------------------------------------------
  point <- rasterToPoints(cov) %>% data.frame
  coordinates(point) <- ~x + y
  projection(point) <- projection(cov)
  projection(station) <- NA
  
  station$residuals[station$residuals > 3.5] <- 3.5
  station$residuals[station$residuals < -3.5] <- -3.5
  
  res1a <- autoKrige(residuals ~ 1, station, new_data = point) 
  
  # Estimate Best Parameter -------------------------------------------------
  
  idpRange <- idpR
  mse <- rep(NA, length(idpRange))
  for (i in 1:length(idpRange)) {
    mse[i] <- mean(krige.cv(residuals ~ 1, station, nfold = nrow(station), set = list(idp = idpRange[i]), verbose = F)$residual^2)
  }
  poss <- which(mse %in% min(mse))
  bestparam <- idpRange[poss]
  residual.best <- krige.cv(residuals ~ 1, station, nfold = nrow(station), set = list(idp = idpRange[poss]), verbose = F)$residual
  
  # Interpolation ----------------------------------------------------------
  
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
  mapa1 <- res1a$krige_output["var1.pred"]
  gridded(mapa1) <- TRUE
  mapa1 <- raster(mapa1)
  projection(mapa1) <- projection(OBSp)
  Ridw1 <- OBSp + mapa1
  # Save Data ---------------------------------------------------------------
  list(Interpol = Ridw, params = list(bestp = bestparam, rmse = sqrt(mean(residual.best^2)),
                                      linear_Model = llm))
}



#RIDW original function made by AybarCL [https://github.com/AybarCL/] 
RIDW_noidpR <- function(gauge, cov, formula, ...) {
  
  cov <- cov[[ formula[2] ]]
  gauge <- gauge[ formula[1] ]
  
  formula <- formula(paste(formula[1], "~", formula[2]))
  
  ####
  
  ext <- raster::extract(cov, gauge, cellnumber = F, sp = T)
  station <- gauge
  linear <- na.omit(ext@data) %>% tbl_df %>% mutate_all(as.character) %>%
    mutate_all(as.numeric)
  llm <- lm(formula,linear)
  station$residuals <- llm$residuals
  
  ####
  
  ext@data %>% 
   plot()
  
  
  ####
  # Define Grid -------------------------------------------------------------
  point <- rasterToPoints(cov) %>% data.frame
  coordinates(point) <- ~x + y
  projection(point) <- projection(cov)

  # Estimate Best Parameter -------------------------------------------------

  # idpRange <- idpR
  # mse <- rep(NA, length(idpRange))
  # for (i in 1:length(idpRange)) {
  #   mse[i] <- mean(krige.cv(residuals ~ 1, station, nfold = nrow(station), set = list(idp = idpRange[i]), verbose = F,...)$residual^2)
  # }
  # poss <- which(mse %in% min(mse))
  # bestparam <- idpRange[poss]
  # residual.best <- krige.cv(residuals ~ 1, station, nfold = nrow(station), set = list(idp = idpRange[poss]), verbose = F,...)$residual

  # Interpolation ----------------------------------------------------------

  idwError <- idw(residuals ~ 1, station, point, idp = 2,...)
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
  # Save Data ---------------------------------------------------------------
  list(Interpol = Ridw)
}
