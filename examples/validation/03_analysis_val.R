library(xts)
library(ggplot2)
library(ggrepel)
library(raster)
library(sp)
library(dplyr)
library(hydroGOF)
library(hexbin)
library(RColorBrewer)

rm(list = ls())

#############

load("./data/shp_dataset.RData")

############

if( get_os() == "windows" ) {
  
  load(file.path("/G:", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","all_gridded_dataset.RData") )
  load(file.path("/G:", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","gridded_dataset.RData") )
  ls()
  
} else if ( get_os() == "linux") {
  
  load(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","all_gridded_dataset.RData") )
  load(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","gridded_dataset.RData") )
  ls()
  
}

#####





rownames(lst_m_mx) <- c(1:12)
rownames(lst_m_mn) <- c(1:12)

order_stats <- xy_stat[order(xy_stat$YY, decreasing = T),]$CC
data_tx <- data_tx[, as.character(order_stats)]
data_tn <- data_tn[, as.character(order_stats)]
lst_m_mn <- lst_m_mn[, as.character(order_stats)]
lst_m_mx <- lst_m_mx[, as.character(order_stats)]
t2mx <- t2mx[, as.character(order_stats)]
t2mn <- t2mn[, as.character(order_stats)]

colnames(data_tx) == colnames(lst_m_mx)
colnames(data_tx) == colnames(t2mx)

############
############ t2mx validation ################
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

hexbinplot(y ~ x, 
           data.frame(x = as.numeric(data_tx),
                      y = as.numeric(t2mx)), 
           xbins = 90 , colramp = rf, 
     xlab = "Temperatura Máxima (°C)",
     ylab = "Temperatura 2m máxima (°C)", 
     xlim = c(0, 40), ylim = c(0, 40),
     aspect = "iso")

#spatial comparison

res_spt_r <- rep(NA, dim(data_tx)[1])

for(j in 1:dim(data_tx)[1])
  {
  
  data_r <-  data.frame(era = as.numeric(t2mx[j,]),
                        obs = as.numeric(data_tx[j,])) %>%
    .[complete.cases(.),]
  
  res_spt_r[j] <- round(cor(data_r)[1,2], 2)  
  }

xts(res_spt_r, time(data_tx)) %>%
  fortify() %>% 
  mutate(Index = as.Date(Index),
         MMM = as.numeric(format(Index, "%m")), 
         Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
                         ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
                                ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
         Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(x = Index, y = ., group = Estación, colour = Estación), size = 2) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen")) + 
  scale_y_continuous(limits = c(-0.4, 1), breaks = seq(-0.4, 1, 0.2), 
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "5 years", limits = c(as.Date("1980-01-01"), as.Date("2016-01-01")), 
               date_labels = "%Y" , expand = c(0,0)) + 
  geom_hline(yintercept = 0, colour = "black") +
  theme_bw() + xlab("") + ylab("R")
  

res_spt_bias <- rep(NA, dim(data_tx)[1])

for(j in 1:dim(data_tx)[1])
{
  
  data_r <-  data.frame(era = as.numeric(t2mx[j,]),
                        obs = as.numeric(data_tx[j,])) %>%
    .[complete.cases(.),]
  
  res_spt_bias[j] <- round(pbias(sim = data_r$era,
                                 obs = data_r$obs), 2)  
  
}


xts(res_spt_bias, time(data_tx)) %>%
  fortify() %>% 
  mutate(Index = as.Date(Index),
         MMM = as.numeric(format(Index, "%m")), 
         Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
                           ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
                                  ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
         Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(x = Index, y = ., group = Estación, colour = Estación), size = 2) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen")) + 
  scale_y_continuous(limits = c(-60, 0), breaks = seq(-60, 0, 5), 
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "5 years", limits = c(as.Date("1980-01-01"), as.Date("2016-01-01")), 
               date_labels = "%Y" , expand = c(0,0)) + 
  geom_hline(yintercept = 0, colour = "black") +
  theme_bw() + xlab("") + ylab("pbias (%)")


#temporal comparison

res_spt_r <- matrix(NA, nrow = 5, ncol = dim(data_tx)[2])

for(j in 1:dim(data_tx)[2])
{
 
 era_data <- t2mx[ ,j] 
 obs_data <- data_tx[ ,j]

 seasons <- list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))

  for(z in 1:length(seasons)){
    
    data_r <- cbind(era_data, obs_data) %>%
      .[as.numeric(format(time(.), "%m")) %in% seasons[[z]]] %>%
      .[complete.cases(.), ]
    
    res_spt_r[z,j]  <- round(cor(data_r)[1,2], 2)  
  }
   
 data_r <-  data.frame(era = as.numeric(era_data),
                        obs = as.numeric(obs_data)) %>%
    .[complete.cases(.),]
  
  res_spt_r[5, j] <- round(cor(data_r)[1,2], 2)  
  
}

colnames(res_spt_r) <- colnames(t2mx)
rownames(res_spt_r) <- c("Verano", "Otoño", "Invierno", "Primavera", "Total")

res_spt_r %>% 
  reshape2::melt() %>%
  mutate(Var2 = factor(Var2, levels = colnames(res_spt_r)),
         Estación = factor(Var1, levels = rownames(res_spt_r))) %>%
  ggplot() + 
  geom_point(aes(x = value, y = Var2, group = Estación, colour = Estación), size = 4) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen","black")) + 
  scale_x_continuous(limits = c(0.2, 0.75), breaks = seq(0.2, 0.75, 0.1), 
                   expand = c(0,0)) +
  geom_vline(xintercept = 0.5, colour = "black") +
  theme_bw() + xlab("R") + ylab("Estaciones meteorologícas")



res_spt_bias <- matrix(NA, nrow = 5, ncol = dim(data_tx)[2])

for(j in 1:dim(data_tx)[2])
{
  
  
  era_data <- t2mx[ ,j] 
  obs_data <- data_tx[ ,j]
  
  seasons <- list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
  
  
  for(z in 1:length(seasons)){
    
    data_r <- cbind(era_data, obs_data) %>%
      .[as.numeric(format(time(.), "%m")) %in% seasons[[z]]] %>%
      .[complete.cases(.), ]
    
    res_spt_bias[z,j]  <- round(pbias(sim = data_r[,1],
                                      obs = data_r[,2]), 2) 
  }
  
  data_r <-  data.frame(era = as.numeric(era_data),
                        obs = as.numeric(obs_data)) %>%
    .[complete.cases(.),]
  
  res_spt_bias[5, j] <- round(pbias(sim = data_r$era,
                                    obs = data_r$obs), 2) 
  
}

colnames(res_spt_bias) <- colnames(t2mx)
rownames(res_spt_bias) <- c("Verano", "Otoño", "Invierno", "Primavera", "Total")

res_spt_bias %>% 
  reshape2::melt() %>%
  mutate(Var2 = factor(Var2, levels = colnames(res_spt_r)),
         Estación = factor(Var1, levels = rownames(res_spt_r))) %>%
  ggplot() + 
  geom_point(aes(x = value, y = Var2, group = Estación, colour = Estación), size = 4) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen","black")) + 
  scale_x_continuous(limits = c(-60, 15), breaks = seq(-60, 10, 10), 
                     expand = c(0,0)) +
  geom_vline(xintercept = 0.5, colour = "black") +
  theme_bw() + xlab("pbias (%)") + ylab("Estaciones meteorologícas")



############
############ t2mn validation ################
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

hexbinplot(y ~ x, 
           data.frame(x = as.numeric(data_tn),
                      y = as.numeric(t2mn)), 
           xbins = 90 , colramp = rf, 
           xlab = "Temperatura Mínima (°C)",
           ylab = "Temperatura 2m mínima (°C)", 
           xlim = c(-20, 30), ylim = c(-20, 30),
           aspect = "iso")

#spatial comparison

res_spt_r <- rep(NA, dim(data_tn)[1])

for(j in 1:dim(data_tn)[1])
{
  
  data_r <-  data.frame(era = as.numeric(t2mn[j,]),
                        obs = as.numeric(data_tn[j,])) %>%
    .[complete.cases(.),]
  
  res_spt_r[j] <- round(cor(data_r)[1,2], 2)  
}

xts(res_spt_r, time(data_tn)) %>%
  fortify() %>% 
  mutate(Index = as.Date(Index),
         MMM = as.numeric(format(Index, "%m")), 
         Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
                           ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
                                  ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
         Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(x = Index, y = ., group = Estación, colour = Estación), size = 2) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen")) + 
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), 
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "5 years", limits = c(as.Date("1980-01-01"), as.Date("2016-01-01")), 
               date_labels = "%Y" , expand = c(0,0)) + 
  geom_hline(yintercept = 0, colour = "black") +
  theme_bw() + xlab("") + ylab("R")


res_spt_bias <- rep(NA, dim(data_tn)[1])

for(j in 1:dim(data_tn)[1])
{
  
  data_r <-  data.frame(era = as.numeric(t2mn[j,]),
                        obs = as.numeric(data_tn[j,])) %>%
    .[complete.cases(.),]
  
  res_spt_bias[j] <- round(pbias(sim = data_r$era,
                                 obs = data_r$obs), 2)  
  
}


xts(res_spt_bias, time(data_tn)) %>%
  fortify() %>% 
  mutate(Index = as.Date(Index),
         MMM = as.numeric(format(Index, "%m")), 
         Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
                           ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
                                  ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
         Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(x = Index, y = ., group = Estación, colour = Estación), size = 2) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen")) + 
  scale_y_continuous(limits = c(-400, 400), breaks = seq(-400, 400, 50), 
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "5 years", limits = c(as.Date("1980-01-01"), as.Date("2016-01-01")), 
               date_labels = "%Y" , expand = c(0,0)) + 
  geom_hline(yintercept = 0, colour = "black") +
  theme_bw() + xlab("") + ylab("pbias (%)")


#temporal comparison

res_spt_r <- matrix(NA, nrow = 5, ncol = dim(data_tn)[2])

for(j in 1:dim(data_tn)[2])
{
  
  era_data <- t2mn[ ,j] 
  obs_data <- data_tn[ ,j]
  
  seasons <- list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
  
  for(z in 1:length(seasons)){
    
    data_r <- cbind(era_data, obs_data) %>%
      .[as.numeric(format(time(.), "%m")) %in% seasons[[z]]] %>%
      .[complete.cases(.), ]
    
    res_spt_r[z,j]  <- round(cor(data_r)[1,2], 2)  
  }
  
  data_r <-  data.frame(era = as.numeric(era_data),
                        obs = as.numeric(obs_data)) %>%
    .[complete.cases(.),]
  
  res_spt_r[5, j] <- round(cor(data_r)[1,2], 2)  
  
}

colnames(res_spt_r) <- colnames(t2mn)
rownames(res_spt_r) <- c("Verano", "Otoño", "Invierno", "Primavera", "Total")

res_spt_r %>% 
  reshape2::melt() %>%
  mutate(Var2 = factor(Var2, levels = colnames(res_spt_r)),
         Estación = factor(Var1, levels = rownames(res_spt_r))) %>%
  ggplot() + 
  geom_point(aes(x = value, y = Var2, group = Estación, colour = Estación), size = 4) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen","black")) + 
  scale_x_continuous(limits = c(0, 0.85), breaks = seq(0, 0.85, 0.1), 
                     expand = c(0,0)) +
  geom_vline(xintercept = 0.5, colour = "black") +
  theme_bw() + xlab("R") + ylab("Estaciones meteorologícas")



res_spt_bias <- matrix(NA, nrow = 5, ncol = dim(data_tn)[2])

for(j in 1:dim(data_tn)[2])
{
  
  
  era_data <- t2mn[ ,j] 
  obs_data <- data_tn[ ,j]
  
  seasons <- list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
  
  
  for(z in 1:length(seasons)){
    
    data_r <- cbind(era_data, obs_data) %>%
      .[as.numeric(format(time(.), "%m")) %in% seasons[[z]]] %>%
      .[complete.cases(.), ]
    
    res_spt_bias[z,j]  <- round(pbias(sim = data_r[,1],
                                      obs = data_r[,2]), 2) 
  }
  
  data_r <-  data.frame(era = as.numeric(era_data),
                        obs = as.numeric(obs_data)) %>%
    .[complete.cases(.),]
  
  res_spt_bias[5, j] <- round(pbias(sim = data_r$era,
                                    obs = data_r$obs), 2) 
  
}

colnames(res_spt_bias) <- colnames(t2mn)
rownames(res_spt_bias) <- c("Verano", "Otoño", "Invierno", "Primavera", "Total")

res_spt_bias %>% 
  reshape2::melt() %>%
  mutate(Var2 = factor(Var2, levels = colnames(res_spt_r)),
         Estación = factor(Var1, levels = rownames(res_spt_r))) %>%
  ggplot() + 
  geom_point(aes(x = value, y = Var2, group = Estación, colour = Estación), size = 4) + 
  scale_colour_manual("",values = c("red", "gray30","dodgerblue3","forestgreen","black")) + 
  scale_x_continuous(limits = c(-400, 400), breaks = seq(-400, 400, 50), 
                     expand = c(0,0)) +
  geom_vline(xintercept = 0.5, colour = "black") +
  theme_bw() + xlab("pbias (%)") + ylab("Estaciones meteorologícas")


############ monthly modis validation ############

data_tn_mclim <- data_tn %>%
  apply(., 2, function(x){
    x <- xts(x, time(data_tn))
    res_clim <- rep(NA, 12)
    
    for(i in 1:12){
      res <- x[as.numeric(format(time(x), "%m")) == i]  
      res_m <- mean(coredata(res), na.rm = T)
      res_clim[i] <- res_m
    }
    
    return(round(res_clim, 2))
    })


data_tx_mclim <- data_tx %>%
  apply(., 2, function(x){
    x <- xts(x, time(data_tx))
    res_clim <- rep(NA, 12)
    
    for(i in 1:12){
      res <- x[as.numeric(format(time(x), "%m")) == i]  
      res_m <- mean(coredata(res), na.rm = T)
      res_clim[i] <- res_m
    }
    
    return(round(res_clim, 2))
  })


####################

lst_m_mn
lst_m_mx
data_tx_mclim
data_tn_mclim


### tx ########
## temporal r ###

res_spt_r <- rep(NA, dim(data_tx_mclim)[1])
res_spt_r_res <- NULL
for(j in 1:dim(data_tx_mclim)[1])
{
  
  data_r <-  data.frame(era = as.numeric(lst_m_mx[j,]),
                        obs = as.numeric(data_tx_mclim[j,])) %>%
    .[complete.cases(.),]
  
  res_spt_r[j] <- round(cor(data_r)[1,2], 2)  
  
  data_r <- data.frame(data_r, month = j)
  res_spt_r_res <- rbind(res_spt_r_res, data_r)
}

xts(res_spt_r, seq(as.Date("1981-01-01"), as.Date("1981-12-01"), by = "month")) %>%
  fortify() %>% 
  # mutate(Index = as.Date(Index),
  #        MMM = as.numeric(format(Index, "%m")), 
  #        Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
  #                          ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
  #                                 ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
  #        Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(x = Index, y = .), size = 5) + 
  scale_y_continuous(limits = c(0.65, 0.75), breaks = seq(0.65, 0.75, 0.05), 
                     expand = c(0,0)) +
  scale_x_date(date_labels = "%m" , expand = c(0,0)) + 
  geom_hline(yintercept = 0, colour = "black") +
  theme_bw() + xlab("") + ylab("R")

#spatial

res_spt_r <- rep(NA, dim(data_tx_mclim)[2])
res_spt_r_res <- NULL
for(j in 1:dim(data_tx_mclim)[2])
{
  
  data_r <-  data.frame(era = as.numeric(lst_m_mx[,j]),
                        obs = as.numeric(data_tx_mclim[,j])) %>%
    .[complete.cases(.),]
  
  res_spt_r[j] <- round(cor(data_r)[1,2], 2)  
  
  data_r <- data.frame(data_r, month = j)
  res_spt_r_res <- rbind(res_spt_r_res, data_r)
}

names(res_spt_r) <- colnames(data_tx_mclim)

res_spt_r %>%
  t() %>%
  reshape2::melt() %>% 
  mutate(Var2 = factor(Var2, levels = names(res_spt_r))) %>%
  # mutate(Index = as.Date(Index),
  #        MMM = as.numeric(format(Index, "%m")), 
  #        Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
  #                          ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
  #                                 ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
  #        Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(y = Var2, x = value), size = 5) + 
  scale_x_continuous(limits = c(-.5, 1), breaks = seq(-.5, 1, 0.1), 
                     expand = c(0,0)) +
  geom_vline(xintercept = 0.5, colour = "black") +
  theme_bw() + xlab("R") + ylab("Estaciones meteorologícas")


### tn ########
## temporal r ###

res_spt_r <- rep(NA, dim(data_tn_mclim)[1])
res_spt_r_res <- NULL
for(j in 1:dim(data_tn_mclim)[1])
{
  
  data_r <-  data.frame(era = as.numeric(lst_m_mn[j,]),
                        obs = as.numeric(data_tn_mclim[j,])) %>%
    .[complete.cases(.),]
  
  res_spt_r[j] <- round(cor(data_r)[1,2], 2)  
  
  data_r <- data.frame(data_r, month = j)
  res_spt_r_res <- rbind(res_spt_r_res, data_r)
}

xts(res_spt_r, seq(as.Date("1981-01-01"), as.Date("1981-12-01"), by = "month")) %>%
  fortify() %>% 
  # mutate(Index = as.Date(Index),
  #        MMM = as.numeric(format(Index, "%m")), 
  #        Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
  #                          ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
  #                                 ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
  #        Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(x = Index, y = .), size = 5) + 
  scale_y_continuous(limits = c(0.7, 0.8), breaks = seq(0.7, 0.8, 0.05), 
                     expand = c(0,0)) +
  scale_x_date(date_labels = "%m" , expand = c(0,0)) + 
  geom_hline(yintercept = 0, colour = "black") +
  theme_bw() + xlab("") + ylab("R")

#spatial

res_spt_r <- rep(NA, dim(data_tn_mclim)[2])
res_spt_r_res <- NULL
for(j in 1:dim(data_tn_mclim)[2])
{
  
  data_r <-  data.frame(era = as.numeric(lst_m_mn[,j]),
                        obs = as.numeric(data_tn_mclim[,j])) %>%
    .[complete.cases(.),]
  
  res_spt_r[j] <- round(cor(data_r)[1,2], 2)  
  
  data_r <- data.frame(data_r, month = j)
  res_spt_r_res <- rbind(res_spt_r_res, data_r)
}

names(res_spt_r) <- colnames(data_tx_mclim)

res_spt_r %>%
  t() %>%
  reshape2::melt() %>% 
  mutate(Var2 = factor(Var2, levels = names(res_spt_r))) %>%
  # mutate(Index = as.Date(Index),
  #        MMM = as.numeric(format(Index, "%m")), 
  #        Estación = ifelse(MMM == 1 | MMM == 12 | MMM == 2, "Verano", 
  #                          ifelse(MMM == 3 | MMM == 4 | MMM == 5, "Otoño",
  #                                 ifelse(MMM == 6 | MMM == 7 | MMM == 8, "Invierno", "Primavera"))),
  #        Estación = factor(Estación, levels = c("Verano", "Otoño", "Invierno", "Primavera"))) %>%
  ggplot() + 
  geom_point(aes(y = Var2, x = value), size = 5) + 
  scale_x_continuous(limits = c(.4, 1), breaks = seq(.4, 1, 0.1), 
                     expand = c(0,0)) +
  geom_vline(xintercept = 0.4, colour = "black") +
  theme_bw() + xlab("R") + ylab("Estaciones meteorologícas")
