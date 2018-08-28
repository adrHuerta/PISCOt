rm(list = ls())

library(readxl) 
library(data.table)
library(dplyr)

############################################# 
##### SENAMHI PERU DATASET
############################################# 

rw_Efiles <- file.path("/media","buntu","D1AB-BCDE","databases","raw_data","data")
rw_Efiles_master <- file.path("/media","buntu","D1AB-BCDE","databases","raw_data","maestro")

### file directories

raw_Efiles <- list.files(path = rw_Efiles, 
                         full.names = T)

raw_Efiles_master <- list.files(path = rw_Efiles_master, 
                                full.names = T)

### reading spatial information ####
 
raw_SInf <- fread(raw_Efiles[9]) %>% 
  mutate(CODIGO = as.character(CODIGO)) %>%
  .[complete.cases(.),] %>% data.table()


raw_master <- fread(raw_Efiles_master[1]) %>% 
  select(CC = Cod_Anti, 
         CC_NEW = Cod_Nuevo, 
         NN = Estacion, 
         DRC = Dre, 
         ZZ = Alt.,
         YY = Lat., XX = Long.)

### merging by CODIGO

raw_SInf_merg <- cbind(raw_SInf,  raw_master[match(raw_SInf$CODIGO, raw_master$CC), ]) %>% 
  select(CC = CODIGO, CC_NEW,  NN, DRE, ZZ, XX, YY) %>%
  mutate(XX = round(XX, 3), YY = round(YY, 3), CC = as.numeric(CC))


### all stations?

dim(raw_SInf_merg)[1] - dim(raw_SInf)[1]

### plot 

raw_SInf_merg %>% 
  plot(YY~XX, data = .)


### reading temporal information ####

raw_Tfiles <- raw_Efiles[1:8] %>% 
  as.list() %>% 
  lapply(. , function(x) fread(x, na.strings = c("", "-999","-888"))) %>%
  do.call("rbind", .) %>% 
  select(CC = V_COD_ESTA, YY = V_ANO, MM = V_MES, DD = V_DIA, TX = N_TM102, TN = N_TM103) %>%
  mutate(CC = factor(CC))

### are similar the codes?

all(levels(raw_Tfiles$CC) == raw_SInf_merg$CC[order(raw_SInf_merg$CC)])

##changing the order according to the order of the code

raw_SInf_merg <- raw_SInf_merg[order(raw_SInf_merg$CC), ]

##creating a list with all data by order of the code

raw_Tfiles_byStation <- raw_Tfiles %>%  
  by(., raw_Tfiles$CC, function(x) {
    
    x %>% 
     # filter(YY >= 1981 & YY < 2016) %>%
      mutate(TT = format(ISOdate(YY, MM, DD), "%Y-%m-%d")) %>%
      select(TT, TX, TN)
    
  })

raw_temp_TX <- lapply(raw_Tfiles_byStation, function(x) {
  x %>% select(TT, TX)
})

raw_temp_TN <- lapply(raw_Tfiles_byStation, function(x) {
  x %>% select(TT, TN)
})

raw_spat_St <- raw_SInf_merg

### looking for NO DATA 

NA_data_TX <- sapply(raw_temp_TX, function(x){ sum(!is.na(x$TX)) }) %>%
  .[. == 0] %>% 
  names()

NA_data_TN <- sapply(raw_temp_TN, function(x){ sum(!is.na(x$TN)) }) %>%
  .[. == 0] %>% 
  names()

#all stations have TN and TX
stat_del <- unique(c(NA_data_TX, NA_data_TN))

lapply(raw_temp_TX[stat_del], function(x){ sum(!is.na(x$TX)) })
lapply(raw_temp_TN[stat_del], function(x){ sum(!is.na(x$TN)) })


#

raw_temp_TX[ stat_del ] <- NULL
raw_temp_TN[ stat_del ] <- NULL


raw_temp_TX <- raw_temp_TX[ !sapply(raw_temp_TX, is.null) ]
raw_temp_TN <- raw_temp_TN[ !sapply(raw_temp_TN, is.null) ]


raw_spat_St <- raw_spat_St[-match(stat_del, raw_spat_St$CC), ]
rownames(raw_spat_St) <- NULL

###

raw_spat_St$CC <- paste("X", raw_spat_St$CC, sep = "")
raw_spat_St$CC_NEW <- paste("X", raw_spat_St$CC_NEW, sep = "")
names(raw_temp_TX) <- paste("X", names(raw_temp_TX), sep = "")
names(raw_temp_TN) <- paste("X", names(raw_temp_TN), sep = "")

all(raw_spat_St$CC == names(raw_temp_TX))
all(raw_spat_St$CC == names(raw_temp_TN))


############################################# 
##### GHCN DATASET
############################################# 

rw_Efiles_gchn_tx <- file.path("/media","buntu","D1AB-BCDE","databases","raw_data","ghcn","tmax_ghcn.txt") %>%
  read.table(.,  header = T, sep = ",")

rw_Efiles_gchn_tn <- file.path("/media","buntu","D1AB-BCDE","databases","raw_data","ghcn","tmin_ghcn.txt") %>%
  read.table(.,  header = T, sep = ",")

rw_Efiles_master_gchn <- file.path("/media","buntu","D1AB-BCDE","databases","raw_data","ghcn","list_stations_ghcn.xls")


###############


raw_spat_St_ghcn <- read_excel(rw_Efiles_master_gchn)  %>% 
  data.frame() %>%
  mutate(CC = paste("X", Code, sep = ""),
         CC_NEW = paste("X", Code, sep = ""), 
         NN = Nombre, 
         DRE = NA,
         ZZ = alt, 
         XX = lon,
         YY = lat) %>%
  select(CC, CC_NEW, NN, DRE, ZZ, XX, YY)


##############

all(colnames(rw_Efiles_gchn_tn) == colnames(rw_Efiles_gchn_tx))

raw_temp_TN_ghcn <- list()
raw_temp_TX_ghcn <- list()
  
for(i in 2:dim(rw_Efiles_gchn_tx)[2]){
  
  df_tn <- data.frame( TT = rw_Efiles_gchn_tn$Index, TN = rw_Efiles_gchn_tn[,i])
  df_tx <- data.frame( TT = rw_Efiles_gchn_tn$Index, TX = rw_Efiles_gchn_tx[,i])
  
  raw_temp_TN_ghcn[[i - 1]] <- df_tn
  raw_temp_TX_ghcn[[i - 1]] <- df_tx
  
}

names(raw_temp_TN_ghcn) <- colnames(rw_Efiles_gchn_tn)[-1]
names(raw_temp_TX_ghcn) <- colnames(rw_Efiles_gchn_tx)[-1]

########################################

raw_temp_TN_ghcn <- raw_temp_TN_ghcn[raw_spat_St_ghcn$CC]
raw_temp_TX_ghcn <- raw_temp_TX_ghcn[raw_spat_St_ghcn$CC]


all(names(raw_temp_TN_ghcn) == raw_spat_St_ghcn$CC)
all(names(raw_temp_TN_ghcn) == raw_spat_St_ghcn$CC)


### merging both dataset

raw_spat_St <- rbind(raw_spat_St, raw_spat_St_ghcn)
raw_temp_TN <- c(raw_temp_TN, raw_temp_TN_ghcn)
raw_temp_TX <- c(raw_temp_TX, raw_temp_TX_ghcn)


all(raw_spat_St$CC == names(raw_temp_TX))
all(names(raw_temp_TN) == names(raw_temp_TX))

### saving data 

  save(raw_temp_TX, raw_temp_TN, raw_spat_St,
       file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step01_RAWDATA.RData"))

