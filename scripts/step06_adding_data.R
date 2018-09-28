rm(list = ls())

library(readxl) 
library(data.table)
library(dplyr)
library(xts)

############################################# 
##### ADDING MORE DATA
############################################# 

load(file.path("/media","buntu","D1AB-BCDE","databases","raw_data","more_data","DATOS_DIARIOS_COMPL_QC_EST_ADEL.RData"))
load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step04_.5_QCDATA_03.RData"))
ls()

file_save0 <- file.path("/media", "buntu","D1AB-BCDE","databases","results","qc","enhanced_qc","adding")

###

raw_temp_TX_7 <- raw_temp_TX_6
raw_temp_TN_7 <- raw_temp_TN_6

#which gauge data is on the original dataset
all_data_tx <- do.call("cbind", all_data_tx)
match_tx <- match(colnames(all_data_tx), colnames(raw_temp_TX_6)) %>% colnames(raw_temp_TX_6)[.] %>% .[!is.na(.)]

all_data_tx[,"X211"][all_data_tx[, "X211"] < 10] <- NA
all_data_tx[,"X640"][all_data_tx[, "X640"] < 5] <- NA
all_data_tx[, "X388"][all_data_tx[, "X388"] < 5] <- NA
all_data_tx[, "X663"][all_data_tx[, "X663"] < 3] <- NA
all_data_tx[, "X727"][all_data_tx[, "X727"] < 10] <- NA
all_data_tx[, "X736"][all_data_tx[, "X736"] < 5] <- NA
all_data_tx[, "X3308"][all_data_tx[, "X3308"] < 5] <- NA
all_data_tx[,"X207"][all_data_tx[, "X207"] < 18] <- NA
all_data_tx[,"X211"][all_data_tx[, "X211"] < 10] <- NA


match_tx <- match_tx[-match(c("X687", "X853"), match_tx)]

for(i in match_tx)
{
  
  act <- all_data_tx[, i] %>% window(start = "2015-01-01")
  oro <- raw_temp_TX_6[, i] %>% window(start = "2015-01-01")
  
  # png(filename = paste(file.path(file_save0, "tx", i), ".png", sep = ""),
  #             width = 1400, height = 1000)
  # 
  # 
  # plot(cbind(act, oro) %>% zoo, type = "p", cex = .1, main = i)
  # 
  # dev.off()
  
  l_act <- sum(!is.na(act))
  l_oro <- sum(!is.na(oro))
  
  if( l_act > l_oro )
    {
    
   window(raw_temp_TX_7[, i], start = "2015-01-01") <- act
    
    }
  
}

raw_temp_TX_7[, "X208"] <- all_data_tx[, "X208"]
raw_temp_TX_7[, "X370"] <- all_data_tx[, "X370"]
#raw_temp_TX_7[, "X640"] <- all_data_tx[, "X640"]
window(raw_temp_TX_7[, "X0024GHCN"], end = "2006-01-01") <- window(raw_temp_TX_7[, "X0024GHCN"], end = "2006-01-01") - 1.25

#no crocodiles in TX from original data!

##################

all_data_tn <- do.call("cbind", all_data_tn)
match_tn <- match(colnames(all_data_tn), colnames(raw_temp_TN_6)) %>% colnames(raw_temp_TN_6)[.] %>% .[!is.na(.)]

all_data_tn[, "X281"][all_data_tn[, "X281"] > 100] <- NA
all_data_tn[, "X321"][all_data_tn[, "X321"] > 50] <- NA
all_data_tn[, "X333"][all_data_tn[, "X333"] > 100] <- NA
all_data_tn[, "X393"][all_data_tn[, "X393"] < 3] <- NA
all_data_tn[, "X341"][all_data_tn[, "X341"] < -5] <- NA
all_data_tn[, "X387"][all_data_tn[, "X387"] > 100] <- NA
all_data_tn[, "X387"][all_data_tn[, "X387"] < 5] <- NA
all_data_tn[, "X388"][all_data_tn[, "X388"] > 15] <- NA
all_data_tn[, "X650"][all_data_tn[, "X650"] > 30] <- NA
all_data_tn[, "X604"][all_data_tn[, "X604"] > 19] <- NA
all_data_tn[, "X837"][all_data_tn[, "X837"] > 30] <- NA
all_data_tn[, "X783"][all_data_tn[, "X783"] < -15] <- NA
all_data_tn[, "X791"][all_data_tn[, "X791"] < 5] <- NA

match_tn <- match_tn[-match(c("X362","X853"), match_tn)]


for(i in match_tn)
{
  
  act <- all_data_tn[, i]  %>% window(start = "2015-01-01")
  oro <- raw_temp_TN_6[, i]  %>% window(start = "2015-01-01")
  
  # png(filename = paste(file.path(file_save0, "tn", i), ".png", sep = ""),
  #     width = 1400, height = 1000)
  # 
  # 
  # plot(cbind(act, oro) %>% zoo, type = "p", cex = .1, main = i)
  # 
  # dev.off()
  
  l_act <- sum(!is.na(act))
  l_oro <- sum(!is.na(oro))
  
  if( l_act > l_oro )
  {
    
    window(raw_temp_TN_7[, i], start = "2015-01-01") <- act
    
  }
}

#some correction that were done before and should be done here to

raw_temp_TN_7[, "X208"] <- all_data_tn[, "X208"]
raw_temp_TN_7[, "X239"] <- all_data_tn[, "X239"]

all_data_tn[, "X882"][time(all_data_tn[, "X882"][is.na(raw_temp_TN_7[, "X882"])])] <- NA
raw_temp_TN_7[, "X882"] <- all_data_tn[, "X882"]


all_data_tn[, "X783"][time(all_data_tn[, "X783"][is.na(raw_temp_TN_7[, "X783"])])] <- NA
raw_temp_TN_7[, "X783"] <- all_data_tn[, "X783"]

plot(raw_temp_TN_7[, "X783"], type = "p", cex = .1)
window(raw_temp_TN_7[, "X783"], start = "2000-01-01")[window(raw_temp_TN_7[, "X783"], start = "2000-01-01") < 0] <-
  window(raw_temp_TN_7[, "X783"], start = "2000-01-01")[window(raw_temp_TN_7[, "X783"], start = "2000-01-01") < 0] + 2
window(raw_temp_TN_7[, "X783"], start = "1983-01-01", end = "1984-12-31")[window(raw_temp_TN_7[, "X783"], start = "1983-01-01", end = "1984-12-31") < 0] <-
  window(raw_temp_TN_7[, "X783"], start = "1983-01-01", end = "1984-12-31")[window(raw_temp_TN_7[, "X783"], start = "1983-01-01", end = "1984-12-31") < 0] + 1

plot(raw_temp_TN_7[, "X783"], type = "p", cex = .1)


plot(raw_temp_TN_7[, "X786"], type = "p", cex = .1)
raw_temp_TN_7[, "X786"][ raw_temp_TN_7[, "X786"] < -12] <-NA
window(raw_temp_TN_7[, "X786"], start = "2015-01-01")[window(raw_temp_TN_7[, "X786"], start = "2015-01-01") < 0] <-
  window(raw_temp_TN_7[, "X786"], start = "2015-01-01")[window(raw_temp_TN_7[, "X786"], start = "2015-01-01") < 0] + 0.65
plot(raw_temp_TN_7[, "X786"], type = "p", cex = .1)

window(raw_temp_TN_7[, "X787"], end = "1988-12-31") <-  NA
window(raw_temp_TN_7[, "X806"], end = "1997-12-31") <-  window(raw_temp_TN_7[, "X806"], end = "1997-12-31") - 0.5

all_data_tn[, "X549"][time(all_data_tn[, "X549"][is.na(raw_temp_TN_7[, "X549"])])] <- NA
raw_temp_TN_7[, "X549"] <- all_data_tn[, "X549"]

all_data_tn[, "X310"][time(all_data_tn[, "X310"][is.na(all_data_tn[, "X310"])])] <- NA
raw_temp_TN_7[, "X310"] <- all_data_tn[, "X310"]

raw_temp_TN_7[, "X727"] <- all_data_tn[, "X727"]

window(raw_temp_TN_7[, "X310"], start = "1992-01-01", end = "1994-12-31") <- NA
window(raw_temp_TN_7[, "X698"], start = "2016-01-01") <- all_data_tn[, "X698"] %>% window(start = "2016-01-01")
raw_temp_TN_7[, "X698"][raw_temp_TN_7[, "X698"] < 4] <- NA

#cocodriles
#in tn there are a lot cocodriles, however the only two after 2015 were corrected!

####################

match_tx <- match(colnames(all_data_tx), colnames(raw_temp_TX_6)) %>% colnames(raw_temp_TX_6)[.] %>% .[!is.na(.)]

# for(i in match_tx)
# {
#   act <- all_data_tx[, i]
#   oro <- raw_temp_TX_6[, i] 
#   act2 <- raw_temp_TX_7[, i]
#   
#   png(filename = paste(file.path(file_save0, "tx", i), ".png", sep = ""),
#               width = 1400, height = 1000)
# 
# 
#   plot(cbind(oro, act, act2) %>% zoo, type = "p", cex = .1, main = i)
# 
#   dev.off()
#   
# }

match_tn <- match(colnames(all_data_tn), colnames(raw_temp_TN_6)) %>% colnames(raw_temp_TN_6)[.] %>% .[!is.na(.)]

# for(i in match_tn)
# {
#   act <- all_data_tn[, i]
#   oro <- raw_temp_TN_6[, i] 
#   act2 <- raw_temp_TN_7[, i]
#   
#   png(filename = paste(file.path(file_save0, "tn", i), ".png", sep = ""),
#       width = 1400, height = 1000)
#   
#   
#   plot(cbind(oro, act, act2) %>% zoo, type = "p", cex = .1, main = i)
#   
#   dev.off()
#   
# }

############################################# 
##### matching stations with TX and TN
############################################# 


match_TX_TN <- c(raw_spat_St_eqc_tn$CC, raw_spat_St_eqc_tx$CC) %>% 
  .[duplicated(.)]

raw_spat_St_eqc <- raw_spat_St_eqc_tn[match(match_TX_TN, raw_spat_St_eqc_tn$CC), ]
raw_temp_TX_7 <- raw_temp_TX_7[, match_TX_TN]
raw_temp_TN_7 <- raw_temp_TN_7[, match_TX_TN]
raw_temp_DTR_7 <- raw_temp_TX_7 - raw_temp_TN_7


### deleting (after spatial interpolation)

raw_spat_St_eqc <- raw_spat_St_eqc[ -match("X790", raw_spat_St_eqc$CC),]
raw_temp_TX_7 <- raw_temp_TX_7[, -match("X790", colnames(raw_temp_TX_7))]
raw_temp_TN_7 <- raw_temp_TN_7[, -match("X790", colnames(raw_temp_TN_7))]
raw_temp_DTR_7 <- raw_temp_DTR_7[, -match("X790", colnames(raw_temp_DTR_7))]


### saving data 

save(raw_temp_TN_7, 
     raw_temp_TX_7, 
     raw_temp_DTR_7,
     raw_spat_St_eqc,
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step05_QCDATA_04.RData"))

