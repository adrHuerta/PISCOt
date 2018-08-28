library(qmap) #install.packages("qmap")
library(xts)
library(dplyr)
library(data.table)

rm(list = ls())

### source codes 

source('./functions/imputation_functions.R')

###

load("./examples/downscaling/prueba_qmTemp.RData")
ls()
###

obs <- prueba[,1] 
mod <- round(prueba[,4], 2)

# res <- cbind(obs, mod) %>%
#   .[complete.cases(.), ]
# 
# plot(zoo(res))
# 
# qm.fit <- fitQmapQUANT(res$X610, res$X22122 ,
#                        qstep = 0.1, nboot = 1, wet.day = F)
# 
# res_qm.fit <- doQmapQUANT(res$X22122, qm.fit, type="linear") %>%
#   xts(., time(res))
# 
# plot(zoo(cbind( res$X610, res_qm.fit)))
# 
# ######################### seasonal

# res_mod_2 <- mod
# 
# res <- cbind(obs, mod) %>%
#   .[complete.cases(.), ]
# 
# plot(zoo(res))
# 
# mm <- list(c("12","01","02"), c("03", "04", "05"),
#            c("06", "07", "08"), c("09", "10","11"))
# names(mm) <- c("summer","autumn","winter","spring")
# 
# for(i in 1:length(mm)){
# 
#   res_As <- res[format(time(res), "%m") %in% mm[[i]]]
#   res_1 <- xts(data.frame(coredata(res_As)), time(res_As))
# 
#   qm.fit <- fitQmapQUANT(res_1[,1], res_1[,2] ,
#                          qstep = 0.01, nboot = 1, wet.day = F, type="tricube")
# 
#   coredata(res_mod_2[format(time(res_mod_2), "%m") %in% mm[[i]] ]) <- round(as.numeric(doQmapQUANT(res_mod_2[format(time(res_mod_2), "%m") %in% mm[[i]] ], qm.fit, type="tricub")), 2)
#   coredata(res[,2][format(time(res[,2]), "%m") %in% mm[[i]] ]) <- round(as.numeric(doQmapQUANT(res_1[,2], qm.fit, type="tricub")), 2)
# 
# }
# 
# plot( zoo( cbind( res[, 1], res_mod_2)) )
# plot( zoo( cbind( res[, 1], res[,2])) )
# qmap_temp(obs_ts = obs, gridded_ts = mod)
# 
# plot( qmap_temp(obs_ts = obs, gridded_ts = mod)$equal_data %>%
#         zoo() )

############# monthly 


obs <- apply(prueba[,1:3], 1, mean, na.rm = T) %>%
  xts(., time(prueba)) 
mod <- round(prueba[,4], 2)

res_mod_2 <- mod

res <- cbind(obs, mod) %>%
  .[complete.cases(.), ]

plot(zoo(res))

mm <- c("01","02", "03", "04", "05","06", "07", "08","09", "10","11", "12")

for(i in 1:length(mm)){
  
  res_As <- res[format(time(res), "%m") %in% mm[[i]]]
  res_1 <- xts(data.frame(coredata(res_As)), time(res_As))
  
  qm.fit <- fitQmapQUANT(res_1[,1], res_1[,2] ,
                         qstep = 0.01, nboot = 1, wet.day = F, type="tricube")
  
  coredata(res_mod_2[format(time(res_mod_2), "%m") %in% mm[[i]] ]) <- round(as.numeric(doQmapQUANT(res_mod_2[format(time(res_mod_2), "%m") %in% mm[[i]] ], qm.fit, type="linear")), 2)
  coredata(res[,2][format(time(res[,2]), "%m") %in% mm[[i]] ]) <- round(as.numeric(doQmapQUANT(res_1[,2], qm.fit, type="linear")), 2)
  
}

plot( zoo( cbind( res[, 1], res_mod_2)) )
plot( zoo( cbind( res[, 1], res[,2])) )
plot( qmap_temp(obs_ts = obs, gridded_ts = mod)$equal_data %>%
      zoo() )

# for(i in 1:length(mm)){
#   
#   res_As <- res[format(time(res), "%m") %in% mm[[i]]]
#   res_1 <- data.frame(coredata(res_As))
#   
#   qm.fit <- fitQmapQUANT(res_1[,1], res_1[,2] ,
#                          qstep = 0.1, nboot = 1, wet.day = F)
#   
#   coredata(mod[format(time(mod), "%m") %in% mm[[i]] ]) <- round(as.numeric(doQmapQUANT(mod[format(time(mod), "%m") %in% mm[[i]] ], qm.fit, type="linear")), 2)
# }
# 
# plot(zoo(cbind( res$X610, mod)))
# 
# 
# #####
# 
# obs <- prueba[,1] 
# mod <- round(prueba[,4], 2)
# 
# 
# plot(zoo(cbind(prueba[,1], qmap_temp(obs_ts = prueba[,1], 
#                                gridded_ts = mod) )))
# qmap_temp(obs_ts = obs, 
#           gridded_ts = mod)
# 
# ######################### monthly
# 
# res_mod_2 <- res$X22122
# 
# mm <- c("01","02", "03", "04", "05","06", "07", "08","09", "10","11", "12")
# 
# for(i in 1:length(mm)){
#   
#   res_As <- res[format(time(res), "%m") %in% mm[[i]]]
#   res_1 <- data.frame(coredata(res_As))
#   
#   qm.fit <- fitQmapQUANT(res_1[,1], res_1[,2] ,
#                          qstep = 0.1, nboot = 1, wet.day = F)
#   
#   coredata(res_mod_2[format(time(res_mod_2), "%m") %in% mm[[i]] ]) <- round(as.numeric(doQmapQUANT(res_1[,2], qm.fit, type="linear")), 2)
# }
# 
# plot(zoo(cbind( res$X610, res_mod_2)))
# 
# 
# ############## considering more than one gauge
# 
# obs <- prueba[,1:3] 
# mod <- round(prueba[,4], 2)
# 
# obs_s <- apply(obs, 1, mean, na.rm = T) %>%
#   xts(., time(obs))
# 
# 
# res <- cbind(dd = obs_s, mod) %>%
#   .[complete.cases(.), ]
# 
# res_mod_2 <- res$X22122
# 
# mm <- list(c("12","01","02"), c("03", "04", "05"), 
#            c("06", "07", "08"), c("09", "10","11"))
# names(mm) <- c("summer","autumn","winter","spring")
# 
# for(i in 1:length(mm)){
#   
#   res_As <- res[format(time(res), "%m") %in% mm[[i]]]
#   res_1 <- data.frame(coredata(res_As))
#   
#   qm.fit <- fitQmapQUANT(res_1[,1], res_1[,2] ,
#                          qstep = 0.1, nboot = 1, wet.day = F)
#   
#   coredata(res_mod_2[format(time(res_mod_2), "%m") %in% mm[[i]] ]) <- round(as.numeric(doQmapQUANT(res_1[,2], qm.fit, type="linear")), 2)
# }
# 
# plot(zoo(cbind( res$dd, res_mod_2)))
