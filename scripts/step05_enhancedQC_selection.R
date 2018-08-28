rm(list = ls())

library(dplyr)
library(data.table)
library(xts)

###

source('./functions/quality_control_temp.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step04_QCDATA_03.RData"))
file_save0 <- file.path("/media", "buntu","D1AB-BCDE","databases","results","qc","enhanced_qc","selection")
file_save1 <- file.path("/media", "buntu","D1AB-BCDE","databases","results","qc","enhanced_qc","comparison")

ls()

raw_temp_TX_5_or <- raw_temp_TX_5
raw_temp_TN_5_or <- raw_temp_TN_5

###

# for(i in 1:length(raw_spat_St_eqc$CC) ){
#   
#   nn <- raw_spat_St_eqc$CC[i]
#   df_p <- cbind(raw_temp_TX_5[, nn], 
#                 raw_temp_TX_5_6[, nn], 
#                 raw_temp_TN_5[, nn], 
#                 raw_temp_TN_5_6[, nn])
#   
#   colnames(df_p) <- c("TX", "TX_enh", 
#                       "TN", "TN_enh")
#   
#   png(filename = paste(file.path(file_save0, nn ), ".png", sep = ""),
#               width = 1400, height = 1000)
# 
# 
#   plot(df_p %>% zoo(), type = "p", cex = .1, main = nn)
# 
#   dev.off()
#   
#   
# }

### first inspection

del_1 <- list.files("/media/buntu/D1AB-BCDE/databases/results/qc/enhanced_qc/selection/del_1") %>% gsub("\\..*","", .)

###

raw_spat_St_eqc <- raw_spat_St_eqc %>%
  .[-match(del_1, .$CC), ]

###

raw_temp_TN_5 <- raw_temp_TN_5 %>% .[, -match(del_1, colnames(raw_temp_TN_5) )]
raw_temp_TX_5 <- raw_temp_TX_5 %>% .[, -match(del_1, colnames(raw_temp_TX_5) )]
raw_temp_TN_5_6 <- raw_temp_TN_5_6 %>% .[, -match(del_1, colnames(raw_temp_TN_5_6) )]
raw_temp_TX_5_6 <- raw_temp_TX_5_6 %>% .[, -match(del_1, colnames(raw_temp_TX_5_6) )]

###
# enhanced evaluation tx

raw_temp_TX_5[, "X0002GHCN"] <- raw_temp_TX_5_6[, "X0002GHCN"] 
raw_temp_TX_5[, "X0003GHCN"] <- raw_temp_TX_5_6[, "X0003GHCN"] 
raw_temp_TX_5[, "X0012GHCN"] <- raw_temp_TX_5_6[, "X0012GHCN"] 
raw_temp_TX_5[, "X0015GHCN"] <- raw_temp_TX_5_6[, "X0015GHCN"] 
raw_temp_TX_5[, "X0017GHCN"] <- raw_temp_TX_5_6[, "X0017GHCN"] 
raw_temp_TX_5[, "X0018GHCN"] <- raw_temp_TX_5_6[, "X0018GHCN"] 
#raw_temp_TX_5[, "X0019GHCN"] <- raw_temp_TX_5_6[, "X0019GHCN"] 
raw_temp_TX_5[, "X0023GHCN"] <- raw_temp_TX_5_6[, "X0023GHCN"] 
raw_temp_TX_5[, "X0024GHCN"] <- raw_temp_TX_5_6[, "X0024GHCN"] 
raw_temp_TX_5[, "X0026GHCN"] <- raw_temp_TX_5_6[, "X0026GHCN"] 
raw_temp_TX_5[, "X0028GHCN"] <- raw_temp_TX_5_6[, "X0028GHCN"] 
raw_temp_TX_5[, "X0029GHCN"] <- raw_temp_TX_5_6[, "X0029GHCN"] 
raw_temp_TX_5[, "X132"] <- raw_temp_TX_5_6[, "X132"] 
raw_temp_TX_5[, "X204"] <- raw_temp_TX_5_6[, "X204"] 
raw_temp_TX_5[, "X208"] <- raw_temp_TX_5_6[, "X208"] 
raw_temp_TX_5[, "X236"] <- raw_temp_TX_5_6[, "X236"] 
window(raw_temp_TX_5[, "X236"], end = "1990-01-01") <- NA
window(raw_temp_TX_5[, "X239"], start = "1982-01-01" , end = "1983-12-31") <- NA
#raw_temp_TX_5[, "X247"] <- raw_temp_TX_5_6[, "X247"] 
raw_temp_TX_5[, "X253"] <- raw_temp_TX_5_6[, "X253"] 
raw_temp_TX_5[, "X269"] <- raw_temp_TX_5_6[, "X269"] 
raw_temp_TX_5[, "X301"] <- raw_temp_TX_5_6[, "X301"] 
window(raw_temp_TX_5[, "X301"], start = "1990-01-01" , end = "1995-12-31") <- NA
window(raw_temp_TX_5[, "X331"], end = "1987-12-31") <- NA
raw_temp_TX_5[, "X335"] <- raw_temp_TX_5_6[, "X335"] 
raw_temp_TX_5[, "X341"] <- raw_temp_TX_5_6[, "X341"] 
window(raw_temp_TX_5[, "X341"], end = "1983-12-31") <- NA
raw_temp_TX_5[, "X370"] <- raw_temp_TX_5_or[, "X370"] 
raw_temp_TX_5[, "X371"] <- raw_temp_TX_5_6[, "X371"] 
raw_temp_TX_5[, "X373"] <- raw_temp_TX_5_6[, "X373"] 
raw_temp_TX_5[, "X374"] <- raw_temp_TX_5_6[, "X374"] 
raw_temp_TX_5[, "X375"] <- raw_temp_TX_5_6[, "X375"] 
window(raw_temp_TX_5[, "X375"], start = "2002-01-01",end = "2008-12-31") <- NA
window(raw_temp_TX_5[, "X378"], end = "1996-12-31") <- NA
raw_temp_TX_5[, "X388"] <- raw_temp_TX_5_or[, "X388"] 
#window(raw_temp_TX_5[, "X388"], end = "1999-12-31") <- NA
raw_temp_TX_5[, "X392"] <- raw_temp_TX_5_6[, "X392"] 
window(raw_temp_TX_5[, "X392"], end = "1999-12-31") <- NA
raw_temp_TX_5[, "X401"] <- raw_temp_TX_5_6[, "X401"] 
raw_temp_TX_5[, "X404"] <- raw_temp_TX_5_6[, "X404"] 
window(raw_temp_TX_5[, "X410"], start = "1990-01-01", end = "1997-12-31") <- NA
raw_temp_TX_5[, "X426"] <- raw_temp_TX_5_6[, "X426"] 
window(raw_temp_TX_5[, "X426"], end = "2000-12-31") <- NA
raw_temp_TX_5[, "X441"] <- raw_temp_TX_5_6[, "X441"] 
window(raw_temp_TX_5[, "X443"], end = "1997-12-31") <- NA
#raw_temp_TX_5[, "X468"] <- raw_temp_TX_5_6[, "X468"] 
raw_temp_TX_5[, "X469"] <- raw_temp_TX_5_6[, "X469"] 
raw_temp_TX_5[, "X478"] <- raw_temp_TX_5_6[, "X478"] 
raw_temp_TX_5[, "X480"] <- raw_temp_TX_5_6[, "X480"] 
raw_temp_TX_5[, "X503"] <- raw_temp_TX_5_6[, "X503"] 
raw_temp_TX_5[, "X528"] <- raw_temp_TX_5_6[, "X528"] 
raw_temp_TX_5[, "X534"] <- raw_temp_TX_5_6[, "X534"] 
raw_temp_TX_5[, "X548"] <- raw_temp_TX_5_6[, "X548"] 
window(raw_temp_TX_5[, "X548"], start = "1991-08-01", end = "1993-12-31") <- NA
raw_temp_TX_5[, "X549"] <- raw_temp_TX_5_6[, "X549"] 
window(raw_temp_TX_5[, "X549"], end = "1986-12-31") <- NA
raw_temp_TX_5[, "X554"] <- raw_temp_TX_5_6[, "X554"] 
raw_temp_TX_5[, "X555"] <- raw_temp_TX_5_6[, "X555"] 
window(raw_temp_TX_5[, "X557"], end = "1995-12-31") <- NA
window(raw_temp_TX_5[, "X590"], end = "2004-12-31") <- NA
window(raw_temp_TX_5[, "X624"], start = "2006-01-01") <- NA
window(raw_temp_TX_5[, "X631"], end = "1993-12-31") <- NA
window(raw_temp_TX_5[, "X633"], end = "1994-12-31") <- NA
window(raw_temp_TX_5[, "X640"], end = "1998-03-31") <- NA
#window(raw_temp_TX_5[, "X650"], end = "1995-12-31") <- NA
raw_temp_TX_5[, "X654"] <- raw_temp_TX_5_6[, "X654"] 
window(raw_temp_TX_5[, "X658"], end = "1990-12-31") <- NA
raw_temp_TX_5[, "X663"] <- raw_temp_TX_5_6[, "X663"] 
raw_temp_TX_5[, "X664"] <- raw_temp_TX_5_6[, "X664"] 
window(raw_temp_TX_5[, "X687"], start = "2014-07-01") <- NA
window(raw_temp_TX_5[, "X687"], start = "1990-01-01", end  = "1991-12-31") <- NA
raw_temp_TX_5[, "X698"] <- raw_temp_TX_5_6[, "X698"] 
raw_temp_TX_5[, "X710"] <- raw_temp_TX_5_6[, "X710"] 
raw_temp_TX_5[, "X736"] <- raw_temp_TX_5_6[, "X736"] 
#window(raw_temp_TX_5[, "X736"], start = "1989-01-01", end = "1993-12-31") <- NA
window(raw_temp_TX_5[, "X741"], end = "1989-01-01") <- NA
window(raw_temp_TX_5[, "X741"], start = "2013-01-01") <- NA
raw_temp_TX_5[, "X743"] <- raw_temp_TX_5_6[, "X743"] 
window(raw_temp_TX_5[, "X743"], end = "1980-01-01") <- NA
#raw_temp_TX_5[, "X749"] <- raw_temp_TX_5_6[, "X749"] 
raw_temp_TX_5[, "X759"] <- raw_temp_TX_5_6[, "X759"] 
raw_temp_TX_5[, "X761"] <- raw_temp_TX_5_6[, "X761"] 
raw_temp_TX_5[, "X781"] <- raw_temp_TX_5_6[, "X781"] 
raw_temp_TX_5[, "X786"] <- raw_temp_TX_5_6[, "X786"] 
window(raw_temp_TX_5[, "X806"], start = "2001-01-01", end = "1998-12-31") <- NA
raw_temp_TX_5[, "X812"] <- raw_temp_TX_5_6[, "X812"] 
window(raw_temp_TX_5[, "X812"], end = "1980-12-31") <- NA
raw_temp_TX_5[, "X815"] <- raw_temp_TX_5_6[, "X815"] 
raw_temp_TX_5[, "X820"] <- raw_temp_TX_5_6[, "X820"] 
raw_temp_TX_5[, "X822"] <- raw_temp_TX_5_6[, "X822"] 
#raw_temp_TX_5[, "X830"] <- raw_temp_TX_5_6[, "X830"] 
raw_temp_TX_5[, "X839"] <- raw_temp_TX_5_6[, "X839"] 
window(raw_temp_TX_5[, "X844"], start = "2001-01-01") <- NA
raw_temp_TX_5[, "X855"] <- raw_temp_TX_5_6[, "X855"] 
window(raw_temp_TX_5[, "X858"], end = "2003-12-31") <- NA
#raw_temp_TX_5[, "X877"] <- raw_temp_TX_5_6[, "X877"] 
window(raw_temp_TX_5[, "X878"], start = "1991-01-01", end = "1995-12-31") <- NA
window(raw_temp_TX_5[, "X879"], start = "2011-01-01", end = "2012-06-30") <- NA
raw_temp_TX_5[, "X889"] <- raw_temp_TX_5_6[, "X889"] 
raw_temp_TX_5[, "X2412"] <- raw_temp_TX_5_6[, "X2412"] 
raw_temp_TX_5[, "X7415"] <- raw_temp_TX_5_6[, "X7415"] 
window(raw_temp_TX_5[, "X157329"], end = "2005-12-31") <- NA
raw_temp_TX_5[, "X158301"] <- raw_temp_TX_5_6[, "X158301"] 
raw_temp_TX_5[, "X158310"] <- raw_temp_TX_5_6[, "X158310"] 
raw_temp_TX_5[, "X158331"] <- raw_temp_TX_5_6[, "X158331"] 

# enhanced evaluation tn

window(raw_temp_TN_5[, "X0002GHCN"], end = "1981-12-31") <- NA
window(raw_temp_TN_5[, "X0002GHCN"], start = "2003-12-31") <- NA
window(raw_temp_TN_5[, "X0003GHCN"], end = "1981-12-31") <- NA
window(raw_temp_TN_5[, "X0003GHCN"], start = "1994-01-01", end = "2003-12-31") <- NA
raw_temp_TN_5[, "X0012GHCN"] <- raw_temp_TN_5_6[, "X0012GHCN"] 
window(raw_temp_TN_5[, "X0015GHCN"], end = "1981-12-31") <- NA
window(raw_temp_TN_5[, "X0015GHCN"], start = "2001-01-01", end = "2003-12-31") <- NA
raw_temp_TN_5[, "X0018GHCN"] <- raw_temp_TN_5_6[, "X0018GHCN"] 
window(raw_temp_TN_5[, "X0019GHCN"], end = "1981-12-31") <- NA
raw_temp_TN_5[, "X0023GHCN"] <- raw_temp_TN_5_6[, "X0023GHCN"] 
raw_temp_TN_5[, "X0024GHCN"] <- raw_temp_TN_5_6[, "X0024GHCN"] 
window(raw_temp_TN_5[, "X0027GHCN"], end = "1981-12-31") <- NA
window(raw_temp_TN_5[, "X0028GHCN"], end = "1981-12-31") <- NA
window(raw_temp_TN_5[, "X0029GHCN"], end = "1981-12-31") <- NA
#raw_temp_TN_5[, "X132"] <- raw_temp_TN_5_6[, "X132"] 
raw_temp_TN_5[, "X204"] <- raw_temp_TN_5_6[, "X204"] 
window(raw_temp_TN_5[, "X236"], end = "1990-01-01") <- NA
window(raw_temp_TN_5[, "X236"], start = "2005-01-01") <- NA
raw_temp_TN_5[, "X239"] <- raw_temp_TN_5_6[, "X239"] 
#raw_temp_TN_5[, "X247"] <- raw_temp_TN_5_6[, "X247"] 
raw_temp_TN_5[, "X250"] <- raw_temp_TN_5_6[, "X250"] 
raw_temp_TN_5[, "X253"] <- raw_temp_TN_5_6[, "X253"] 
raw_temp_TN_5[, "X301"] <- raw_temp_TN_5_6[, "X301"] 
window(raw_temp_TN_5[, "X301"], start = "1990-01-01" , end = "1995-12-31") <- NA
raw_temp_TN_5[, "X331"] <- raw_temp_TN_5_6[, "X331"] 
raw_temp_TN_5[, "X335"] <- raw_temp_TN_5_6[, "X335"] 
raw_temp_TN_5[, "X341"] <- raw_temp_TN_5_6[, "X341"] 
raw_temp_TN_5[, "X371"] <- raw_temp_TN_5_6[, "X371"] 
raw_temp_TN_5[, "X374"] <- raw_temp_TN_5_6[, "X374"] 
window(raw_temp_TN_5[, "X374"], start = "1992-01-01", end = "1994-12-31") <- NA
raw_temp_TN_5[, "X375"] <- raw_temp_TN_5_6[, "X375"] 
window(raw_temp_TN_5[, "X378"], end = "1991-01-01") <- NA
raw_temp_TN_5[, "X388"] <- raw_temp_TN_5_6[, "X388"] 
raw_temp_TN_5[, "X391"] <- raw_temp_TN_5_6[, "X391"] 
window(raw_temp_TN_5[, "X392"], end = "2005-12-31") <- NA
raw_temp_TN_5[, "X404"] <- raw_temp_TN_5_6[, "X404"] 
raw_temp_TN_5[, "X410"] <- raw_temp_TN_5_6[, "X410"] 
raw_temp_TN_5[, "X426"] <- raw_temp_TN_5_6[, "X426"] 
window(raw_temp_TN_5[, "X435"], start = "2015-01-01") <- NA
raw_temp_TN_5[, "X459"] <- raw_temp_TN_5_6[, "X459"] 
raw_temp_TN_5[, "X468"] <- raw_temp_TN_5_6[, "X468"] 
raw_temp_TN_5[, "X469"] <- raw_temp_TN_5_6[, "X469"] 
raw_temp_TN_5[, "X478"] <- raw_temp_TN_5_6[, "X478"] 
window(raw_temp_TN_5[, "X480"], end = "2001-12-31") <- NA
raw_temp_TN_5[, "X503"] <- raw_temp_TN_5_6[, "X503"] 
raw_temp_TN_5[, "X528"] <- raw_temp_TN_5_6[, "X528"] 
raw_temp_TN_5[, "X534"] <- raw_temp_TN_5_6[, "X534"] 
window(raw_temp_TN_5[, "X539"], start = "2009-01-01", end = "2010-12-31") <- NA
window(raw_temp_TN_5[, "X540"],  end = "1991-12-31") <- NA
window(raw_temp_TN_5[, "X548"],  end = "1983-12-31") <- NA
raw_temp_TN_5[, "X549"] <- raw_temp_TN_5_6[, "X549"] 
#window(raw_temp_TN_5[, "X549"],  end = "1983-12-31") <- NA
window(raw_temp_TN_5[, "X590"],  end = "2004-12-31") <- NA
window(raw_temp_TN_5[, "X604"],  end = "1997-12-31") <- NA
window(raw_temp_TN_5[, "X605"],  start = "2012-01-01") <- NA
raw_temp_TN_5[, "X624"] <- raw_temp_TN_5_6[, "X624"] 
window(raw_temp_TN_5[, "X605"],  end = "1994-12-31") <- NA
window(raw_temp_TN_5[, "X640"], end = "1997-12-31") <- NA
#window(raw_temp_TN_5[, "X650"], end = "1995-12-31") <- NA
raw_temp_TN_5[, "X659"] <- raw_temp_TN_5_6[, "X659"] 
raw_temp_TN_5[, "X664"] <- raw_temp_TN_5_6[, "X664"] 
window(raw_temp_TN_5[, "X687"], start = "2015-01-01") <- NA
raw_temp_TN_5[, "X698"] <- raw_temp_TN_5_6[, "X698"] 
raw_temp_TN_5[, "X710"] <- raw_temp_TN_5_6[, "X710"] 
window(raw_temp_TN_5[, "X730"], start = "1990-01-01", end = "1993-12-31") <- NA
raw_temp_TN_5[, "X736"] <- raw_temp_TN_5_6[, "X736"] 
raw_temp_TN_5[, "X741"] <- raw_temp_TN_5_6[, "X741"] 
raw_temp_TN_5[, "X743"] <- raw_temp_TN_5_6[, "X743"] 
raw_temp_TN_5[, "X749"] <- raw_temp_TN_5_6[, "X749"] 
raw_temp_TN_5[, "X761"] <- raw_temp_TN_5_6[, "X761"] 
raw_temp_TN_5[, "X788"] <- raw_temp_TN_5_6[, "X788"] 
raw_temp_TN_5[, "X793"] <- raw_temp_TN_5_6[, "X793"] 
#window(raw_temp_TN_5[, "X806"], start = "1993-01-01", end = "1997-12-31") <- NA
raw_temp_TN_5[, "X812"] <- raw_temp_TN_5_6[, "X812"] 
raw_temp_TN_5[, "X815"] <- raw_temp_TN_5_6[, "X815"] 
raw_temp_TN_5[, "X820"] <- raw_temp_TN_5_6[, "X820"] 
raw_temp_TN_5[, "X822"] <- raw_temp_TN_5_6[, "X822"] 
raw_temp_TN_5[, "X882"] <- raw_temp_TN_5_6[, "X882"] 
raw_temp_TN_5[, "X889"] <- raw_temp_TN_5_6[, "X889"] 
raw_temp_TN_5[, "X2412"] <- raw_temp_TN_5_6[, "X2412"] 
raw_temp_TN_5[, "X4450"] <- raw_temp_TN_5_6[, "X4450"] 
window(raw_temp_TN_5[, "X7454"], end = "2003-12-31") <- NA
raw_temp_TN_5[, "X113162"] <- raw_temp_TN_5_6[, "X113162"] 
raw_temp_TN_5[, "X158301"] <- raw_temp_TN_5_6[, "X158301"] 
raw_temp_TN_5[, "X0017GHCN"] <- raw_temp_TN_5_6[, "X0017GHCN"]

#crocodiles :(
window(raw_temp_TN_5[, "X778"], start = "2003-08-01", end = "2014-10-31")[window(raw_temp_TN_5[, "X778"], start = "2003-08-01", end = "2014-10-31") < 0]  <- 
  window(raw_temp_TN_5[, "X778"], start = "2003-08-01", end = "2014-10-31")[window(raw_temp_TN_5[, "X778"], start = "2003-08-01", end = "2014-10-31") < 0] + 4
window(raw_temp_TN_5[, "X816"], start = "1998-05-01", end = "2011-10-31")[window(raw_temp_TN_5[, "X816"], start = "1998-05-01", end = "2011-10-31") < 0] <- 
  window(raw_temp_TN_5[, "X816"], start = "1998-05-01", end = "2011-10-31")[window(raw_temp_TN_5[, "X816"], start = "1998-05-01", end = "2011-10-31") < 0] + 4

### second inspection

del_2 <- list.files("/media/buntu/D1AB-BCDE/databases/results/qc/enhanced_qc/selection/del_2/cannot_be_solved/") %>% 
  gsub("\\..*","", .)

del_2_specials <- list.files("/media/buntu/D1AB-BCDE/databases/results/qc/enhanced_qc/selection/del_2/specials/") %>% 
  gsub("\\..*","", .)

del_2 <- c(del_2, del_2_specials)
  
###

raw_spat_St_eqc <- raw_spat_St_eqc %>%
  .[-match(del_2, .$CC), ]

###

raw_temp_TN_5 <- raw_temp_TN_5 %>% .[, -match(del_2, colnames(raw_temp_TN_5) )]
raw_temp_TX_5 <- raw_temp_TX_5 %>% .[, -match(del_2, colnames(raw_temp_TX_5) )]
raw_temp_TN_5_6 <- raw_temp_TN_5_6 %>% .[, -match(del_2, colnames(raw_temp_TN_5_6) )]
raw_temp_TX_5_6 <- raw_temp_TX_5_6 %>% .[, -match(del_2, colnames(raw_temp_TX_5_6) )]

###
window(raw_temp_TX_5[, "X0004GHCN"],  end = "1981-12-31") <- NA
raw_temp_TX_5[, "X176"] <- raw_temp_TX_5_6[, "X176"]
window(raw_temp_TX_5[, "X176"],  start = "1993-01-01",end = "1993-12-31") <- NA
window(raw_temp_TX_5[, "X313"],  end = "1999-12-31") <- NA
window(raw_temp_TX_5[, "X332"],  start = "2001-01-01",end = "2005-12-31") <- NA
window(raw_temp_TX_5[, "X359"],  end = "1998-12-31") <- NA
#window(raw_temp_TX_5[, "X359"],  end = "2015-07-31")  <- window(raw_temp_TX_5[, "X359"],  end = "2015-07-31") + 0.75
window(raw_temp_TX_5[, "X556"], end = "2000-12-31") <- NA
window(raw_temp_TX_5[, "X541"],  start ="1991-01-01", end = "1993-12-31") <- NA
window(raw_temp_TX_5[, "X541"],  start ="2015-01-01") <- NA
raw_temp_TX_5[, "X552"] <- raw_temp_TX_5_6[, "X552"]
window(raw_temp_TX_5[, "X552"],  start = "2001-01-01", end = "2003-12-31") <- NA
window(raw_temp_TX_5[, "X572"], end = "2000-12-31") <- NA
raw_temp_TX_5[, "X657"] <- raw_temp_TX_5_6[, "X657"]
window(raw_temp_TX_5[, "X669"], end = "1984-12-31") <- NA
# raw_temp_TX_5[, "X783"] <- raw_temp_TX_5_6[, "X783"]
# window(raw_temp_TX_5[, "X783"], start = "2013-12-31") <- NA
raw_temp_TX_5[, "X823"] <- raw_temp_TX_5_6[, "X823"]
window(raw_temp_TX_5[, "X853"],  start = "2010-01-01") <- NA
window(raw_temp_TX_5[, "X881"],  start = "2006-01-01", end = "2011-12-31") <- NA
window(raw_temp_TX_5[, "X881"],  end = "1993-06-30") <- NA

window(raw_temp_TN_5[, "X0004GHCN"],  end = "1981-12-31") <- NA
window(raw_temp_TN_5[, "X176"],  start = "1992-01-01",end = "1997-12-31") <- NA
window(raw_temp_TN_5[, "X332"], start = "2000-01-01" , end = "2005-12-31") <- NA
window(raw_temp_TN_5[, "X359"], end = "1996-12-31") <- NA
#window(raw_temp_TN_5[, "X359"],  start ="2015-01-01") <- NA
window(raw_temp_TN_5[, "X552"],  end = "2004-12-31") <- NA
raw_temp_TN_5[, "X556"] <- raw_temp_TN_5_6[, "X556"]
window(raw_temp_TN_5[, "X572"], end = "1989-12-31") <- NA
window(raw_temp_TN_5[, "X669"], end = "1984-12-31") <- NA
window(raw_temp_TN_5[, "X853"],  start = "2010-01-01") <- NA
window(raw_temp_TN_5[, "X881"],  start = "2006-01-01", end = "2011-12-31") <- NA
window(raw_temp_TN_5[, "X881"],  end = "1993-06-30") <- NA

###

del_3_just_tn <- list.files("/media/buntu/D1AB-BCDE/databases/results/qc/enhanced_qc/selection/del_2/can_be_solved/just_tn/") %>% 
  gsub("\\..*","", .)

raw_temp_TN_5[, "X310"] <- raw_temp_TN_5_6[, "X310"]
window(raw_temp_TN_5[, "X310"], start = "1992-01-01", end = "1994-12-31") <- NA
raw_temp_TN_5[, "X321"] <- raw_temp_TN_5_6[, "X321"]
window(raw_temp_TN_5[, "X321"], end = "1992-01-01") <- NA
raw_temp_TN_5[, "X476"] <- raw_temp_TN_5_6[, "X476"]
window(raw_temp_TN_5[, "X476"], start = "2010-01-01") <- NA
raw_temp_TN_5[, "X751"] <- raw_temp_TN_5_6[, "X751"]
window(raw_temp_TN_5[, "X751"], end = "2002-12-31") <- NA
window(raw_temp_TN_5[, "X780"], end = "1985-12-31") <- NA


del_3_just_tx <- list.files("/media/buntu/D1AB-BCDE/databases/results/qc/enhanced_qc/selection/del_2/can_be_solved/just_tx/") %>% 
  gsub("\\..*","", .)

raw_temp_TX_5[, "X0010GHCN"] <- raw_temp_TX_5_6[, "X0010GHCN"]
window(raw_temp_TX_5[, "X172"],  end = "1989-12-31") <- NA
window(raw_temp_TX_5[, "X180"],  end = "1992-12-31") <- NA
raw_temp_TX_5[, "X446"] <- raw_temp_TX_5_6[, "X446"]
window(raw_temp_TX_5[, "X446"],  end = "2000-01-01") <- NA

### 

#after final view

window(raw_temp_TN_5[, "X238"], start = "2010-05-01", end = "2010-12-31") <- NA
window(raw_temp_TN_5[, "X250"], start = "2000-07-01", end = "2002-07-31")[window(raw_temp_TN_5[, "X250"], start = "2000-07-01", end = "2002-07-31") < 14] <- 
  window(raw_temp_TN_5[, "X250"], start = "2000-07-01", end = "2002-07-31")[window(raw_temp_TN_5[, "X250"], start = "2000-07-01", end = "2002-07-31") < 14] + 3
window(raw_temp_TN_5[, "X250"], start = "1998-01-01", end = "1998-08-31") <- NA
window(raw_temp_TN_5[, "X250"], start = "2007-06-01", end = "2008-08-31")[window(raw_temp_TN_5[, "X250"], start = "2007-06-01", end = "2008-08-31") < 16] <- 
  window(raw_temp_TN_5[, "X250"], start = "2007-06-01", end = "2008-08-31")[window(raw_temp_TN_5[, "X250"], start = "2007-06-01", end = "2008-08-31") < 16] + 1
window(raw_temp_TN_5[, "X253"], start = "1999-01-01", end = "2001-12-31") <- NA
window(raw_temp_TN_5[, "X253"], start = "2007-01-01", end = "2009-12-31")<- NA
window(raw_temp_TN_5[, "X269"], start = "2010-07-01", end = "2011-06-30")[window(raw_temp_TN_5[, "X269"], start = "2010-07-01", end = "2011-06-30") < 21] <- 
  window(raw_temp_TN_5[, "X269"], start = "2010-07-01", end = "2011-06-30")[window(raw_temp_TN_5[, "X269"], start = "2010-07-01", end = "2011-06-30") < 21] + 3
window(raw_temp_TN_5[, "X269"], start = "2013-07-01", end = "2014-06-30")[window(raw_temp_TN_5[, "X269"], start = "2013-07-01", end = "2014-06-30") < 21] <- 
  window(raw_temp_TN_5[, "X269"], start = "2013-07-01", end = "2014-06-30")[window(raw_temp_TN_5[, "X269"], start = "2013-07-01", end = "2014-06-30") < 21] + 3
window(raw_temp_TN_5[, "X282"], start = "1984-01-31", end = "1987-03-30") <- NA
window(raw_temp_TN_5[, "X319"], end = "2007-11-01")[window(raw_temp_TN_5[, "X319"], end = "2007-11-01") < 12] <-
  window(raw_temp_TN_5[, "X319"], end = "2007-11-01")[window(raw_temp_TN_5[, "X319"], end = "2007-11-01") < 12] + 4
#window(raw_temp_TN_5[, "X333"], start = "2009-01-01", end = "2014-06-30") <- NA
window(raw_temp_TN_5[, "X343"], end =  "1988-12-31") <- NA
window(raw_temp_TN_5[, "X362"], start =  "1994-01-31", end = "1998-01-01")[window(raw_temp_TN_5[, "X362"], start =  "1994-01-31", end = "1998-01-01") < 8] <- 
  window(raw_temp_TN_5[, "X362"], start =  "1994-01-31", end = "1998-01-01")[window(raw_temp_TN_5[, "X362"], start =  "1994-01-31", end = "1998-01-01") < 8] + 6 
window(raw_temp_TN_5[, "X393"], start = "1992-01-01", end =  "1997-12-31") <- NA
window(raw_temp_TN_5[, "X393"], start = "2011-01-01", end =  "2012-06-30") <- NA 
window(raw_temp_TN_5[, "X393"], start = "2014-01-01", end =  "2014-12-31") <- NA 
raw_temp_TN_5[, "X501"][raw_temp_TN_5[, "X501"] < 5] <- NA
raw_temp_TN_5[, "X501"][raw_temp_TN_5[, "X501"] > 30] <- NA
raw_temp_TN_5[, "X539"][raw_temp_TN_5[, "X539"] < 5] <- NA
window(raw_temp_TN_5[, "X625"], start = "1990-01-01", end = "2001-12-31")[window(raw_temp_TN_5[, "X625"], start = "1990-01-01", end = "2001-12-31") < 0 ] <- 
  window(raw_temp_TN_5[, "X625"], start = "1990-01-01", end = "2001-12-31")[window(raw_temp_TN_5[, "X625"], start = "1990-01-01", end = "2001-12-31") < 0] + 1
raw_temp_TN_5[, "X631"][raw_temp_TN_5[, "X631"] < 9] <- NA
window(raw_temp_TN_5[, "X633"], end = "1995-06-30") <- NA
window(raw_temp_TN_5[, "X633"], start = "2001-06-01", end = "2005-07-31") <- NA
raw_temp_TN_5[, "X638"][raw_temp_TN_5[, "X638"] < 5] <- NA
window(raw_temp_TN_5[, "X657"], end = "1984-12-31") <- NA
window(raw_temp_TN_5[, "X657"], start = "1994-01-01", end = "1995-12-31") <- NA
window(raw_temp_TN_5[, "X658"], start = "2004-07-01", end = "2011-07-31")[ window(raw_temp_TN_5[, "X658"], start = "2004-07-01", end = "2011-07-31") < 10] <- 
  window(raw_temp_TN_5[, "X658"], start = "2004-07-01", end = "2011-07-31")[ window(raw_temp_TN_5[, "X658"], start = "2004-07-01", end = "2011-07-31") < 10] + 1 
window(raw_temp_TN_5[, "X658"], end = "1984-07-01") <- NA
window(raw_temp_TN_5[, "X658"], start = "2009-01-01", end = "2010-12-31") <- NA
window(raw_temp_TN_5[, "X790"], start = "2008-07-07", end = "2014-06-30")[window(raw_temp_TN_5[, "X790"], start = "2008-07-07", end = "2014-06-30") < 26 ] <- 
  window(raw_temp_TN_5[, "X790"], start = "2008-07-07", end = "2014-06-30")[window(raw_temp_TN_5[, "X790"], start = "2008-07-07", end = "2014-06-30") < 26] - 2
window(raw_temp_TN_5[, "X790"], start = "2012-07-07", end = "2014-06-30")[window(raw_temp_TN_5[, "X790"], start = "2012-07-07", end = "2014-06-30") < 25 ] <- 
  window(raw_temp_TN_5[, "X790"], start = "2012-07-07", end = "2014-06-30")[window(raw_temp_TN_5[, "X790"], start = "2012-07-07", end = "2014-06-30") < 25] - 1.5
raw_temp_TN_5[, "X557"][raw_temp_TN_5[, "X557"] > 20] <- NA

raw_temp_TX_5[, "X0027GHCN"][raw_temp_TX_5[, "X0027GHCN"] > 45] <- NA
raw_temp_TX_5[, "X135"][raw_temp_TX_5[, "X135"] > 37] <- NA
window(raw_temp_TX_5[, "X269"], end = "1996-12-31") <- NA
window(raw_temp_TX_5[, "X353"], start = "1984-01-01",end = "1986-12-31") <- NA
window(raw_temp_TX_5[, "X359"], start = "2015-07-01")[window(raw_temp_TX_5[, "X359"], start = "2015-07-01") > 12] <-
  window(raw_temp_TX_5[, "X359"], start = "2015-07-01")[window(raw_temp_TX_5[, "X359"], start = "2015-07-01") > 12] - 2
window(raw_temp_TX_5[, "X359"], start = "2009-07-01", end = "2010-12-31")[window(raw_temp_TX_5[, "X359"], start = "2009-07-01", end = "2010-12-31") < 23] <- 
  window(raw_temp_TX_5[, "X359"], start = "2009-07-01", end = "2010-12-31")[window(raw_temp_TX_5[, "X359"], start = "2009-07-01", end = "2010-12-31") < 23] - 1
window(raw_temp_TX_5[, "X441"], start = "2012-01-01", end = "2013-06-30") <- NA
window(raw_temp_TX_5[, "X0019GHCN"], end =  "1981-12-31") <- NA
raw_temp_TX_5[, "X373"][raw_temp_TX_5[, "X373"] > 31] <- NA
window(raw_temp_TX_5[, "X373"], start = "2015-07-01")[window(raw_temp_TX_5[, "X373"], start = "2015-07-01") > 16] <- 
  window(raw_temp_TX_5[, "X373"], start = "2015-07-01")[window(raw_temp_TX_5[, "X373"], start = "2015-07-01") > 16] - 1
window(raw_temp_TX_5[, "X373"], end =  "1990-01-01") <- NA
window(raw_temp_TX_5[, "X445"], end = "1986-12-31") <- NA
window(raw_temp_TX_5[, "X503"], end = "1987-12-31") <- NA
raw_temp_TX_5[, "X503"][raw_temp_TX_5[, "X503"] > 30] <- NA
window(raw_temp_TX_5[, "X790"], start = "2009-07-01", end = "2014-07-31")[window(raw_temp_TX_5[, "X790"], start = "2009-07-01", end = "2014-07-31") > 10] <-
  window(raw_temp_TX_5[, "X790"], start = "2009-07-01", end = "2014-07-31")[window(raw_temp_TX_5[, "X790"], start = "2009-07-01", end = "2014-07-31") > 10] - 2
window(raw_temp_TX_5[, "X806"], start = "1990-06-01", end = "1992-12-31") <- NA
raw_temp_TX_5[, "X811"][raw_temp_TX_5[, "X811"] > 32] <- NA
raw_temp_TX_5[, "X832"][raw_temp_TX_5[, "X832"] < 12] <- NA
raw_temp_TX_5[, "X877"][raw_temp_TX_5[, "X877"] > 30] <- NA
window(raw_temp_TX_5[, "X879"], end = "1986-12-31") <- NA
window(raw_temp_TX_5[, "X880"], start = "1990-01-01",end = "1990-06-30") <- NA

raw_temp_TX_5[, "X0003GHCN"] <- raw_temp_TX_5_or[, "X0003GHCN"]
window(raw_temp_TX_5[, "X0003GHCN"], end = "1981-12-31") <- NA
raw_temp_TX_5[, "X0012GHCN"] <- raw_temp_TX_5_or[, "X0012GHCN"]
window(raw_temp_TX_5[, "X0012GHCN"], end = "1981-12-31") <- NA
raw_temp_TX_5[, "X0029GHCN"] <- raw_temp_TX_5_or[, "X0029GHCN"]
window(raw_temp_TX_5[, "X0029GHCN"], end = "1981-12-31") <- NA
window(raw_temp_TX_5[, "X736"], end = "1985-12-31") <- NA
raw_temp_TX_5[, "X749"] <- raw_temp_TX_5_or[, "X749"]
window(raw_temp_TX_5[, "X749"], start = "1992-01-01", end = "1992-12-31") <- NA
window(raw_temp_TX_5[, "X749"], start = "1982-01-01", end = "1982-12-31") <- NA



###

raw_spat_St_eqc_tx <- raw_spat_St_eqc %>%
  .[-match(del_3_just_tn, .$CC), ]

raw_temp_TX_6 <- raw_temp_TX_5[, raw_spat_St_eqc_tx$CC]
length_of_tx <- apply(raw_temp_TX_6, 2, filter_qc) %>%
  .[. >= 15] %>%
  names()

raw_temp_TX_6 <- raw_temp_TX_6[, c(length_of_tx, "X590")]
raw_spat_St_eqc_tx <- raw_spat_St_eqc_tx[match(c(length_of_tx, "X590"), raw_spat_St_eqc_tx$CC), ]

###

raw_spat_St_eqc_tn <- raw_spat_St_eqc %>%
  .[-match(del_3_just_tx, .$CC), ]

raw_temp_TN_6 <- raw_temp_TN_5[, raw_spat_St_eqc_tn$CC]
length_of_tn <- apply(raw_temp_TN_6, 2, filter_qc) %>%
  .[. >= 15] %>%
  names()

raw_temp_TN_6 <- raw_temp_TN_6[, c(length_of_tn, "X590")]
raw_spat_St_eqc_tn <- raw_spat_St_eqc_tn[match(c(length_of_tn, "X590"), raw_spat_St_eqc_tn$CC), ]


### Comparison

# for(z in 1:dim(raw_temp_TX_6)[2]){
# 
#   nnn <- colnames(raw_temp_TX_6)[z]
#   df_p <- cbind(raw_temp_TX_6[, nnn],
#                 raw_temp_TX_5_6[, nnn],
#                 raw_temp_TX_5_or[, nnn])
# 
#      colnames(df_p) <- c("EQC", "EQC_O", "QC")
# 
#      png(filename = paste(file.path(file_save1, "tx", nnn), ".png", sep = ""),
#                  width = 1400, height = 1000)
# 
# 
#      plot(df_p %>% zoo(), type = "p", cex = .1, main = nnn)
# 
#      dev.off()
#   }
# 
# 
# for(z in 1:dim(raw_temp_TN_6)[2]){
# 
#   nnn <- colnames(raw_temp_TN_6)[z]
#   df_p <- cbind(raw_temp_TN_6[, nnn],
#                 raw_temp_TN_5_6[, nnn],
#                 raw_temp_TN_5_or[, nnn])
# 
#   colnames(df_p) <- c("EQC", "EQC_O", "QC")
# 
#   png(filename = paste(file.path(file_save1, "tn", nnn), ".png", sep = ""),
#       width = 1400, height = 1000)
# 
# 
#   plot(df_p %>% zoo(), type = "p", cex = .1, main = nnn)
# 
#   dev.off()
# }


### saving data 

save(raw_temp_TN_6, raw_temp_TX_6, 
     raw_spat_St_eqc_tn, raw_spat_St_eqc_tx,
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step04_.5_QCDATA_03.RData"))

# examples :(
raw_temp_TX_6[,"X378"] %>% plot(type = "p", cex = .1)
raw_temp_TX_5_or[,"X378"] %>% plot(type = "p", cex = .1)

raw_temp_TN_5_or[,"X790"] %>% plot(type = "p", cex = .1)
raw_temp_TN_6[,"X790"] %>% plot(type = "p", cex = .1)

raw_temp_TN_6[,"X778"] %>% plot(type = "p", cex = .1)
raw_temp_TN_5_or[,"X778"] %>% plot(type = "p", cex = .1)

raw_temp_TN_5_or[,"X362"] %>% plot(type = "p", cex = .1)
raw_temp_TN_6[,"X362"] %>% plot(type = "p", cex = .1)
