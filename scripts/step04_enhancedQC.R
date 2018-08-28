rm(list = ls())

library(dplyr)
library(data.table)
library(xts)

### source codes 

source('./functions/enhanced_qc.R')

###

load(file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step03_QCDATA_02.RData"))
file_save0 <- file.path("/media", "buntu","D1AB-BCDE","databases","results","qc","enhanced_qc","plots")
ls()


# plot(raw_temp_TX_5[c("X590", "X808","X0011GHCN","X0027GHCN")] %>% 
#        do.call("cbind", .) %>% 
#        zoo(), plot.type = "multiple", type = "p", cex = 0.1)
###

raw_spat_St_eqc <- raw_spat_St

raw_temp_TN_5_6 <- list()
raw_temp_TX_5_6 <- list()

for( j in 1:length(raw_temp_TN_5) ){
  
  tx_ts <- raw_temp_TX_5[[j]]
  tn_ts <- raw_temp_TN_5[[j]]
  
  if( sum(!is.na(tx_ts)) < 500 ){
    
    tx_ts_eqc <- tx_ts
    
  # } else if () {
  #   
  #   tx_ts_eqc <- NA
  #   
  } else {
    
  tx_ts_eqc <- enhanced_qc(tx_ts)
  
  if( all(is.na(tx_ts_eqc)) == T ){
    
    tx_ts_eqc <- tx_ts
    
  } 
  
  }
  
  
  if( sum(!is.na(tn_ts)) < 500 ){
    
    tn_ts_eqc <- NA
    
    # } else if () {
    #   
    #   tx_ts_eqc <- NA
    #   
  } else {
    
    tn_ts_eqc <- enhanced_qc(tn_ts)
    
    if( all(is.na(tn_ts_eqc)) == T ){
      
      tn_ts_eqc <- tn_ts
      
    } 
    
  }
  
 
 # if( all(is.na(tn_ts_eqc)) != T & all(is.na(tx_ts_eqc)) != T) {
 # 
 # png(filename = paste(file.path(file_save0, names(raw_temp_TN_5)[[j]] ), ".png", sep = ""),
 #             width = 1400, height = 1000)
 # 
 # df_pt  <- cbind(tn_ts_eqc, tn_ts, tx_ts_eqc, tx_ts) %>% zoo()
 # colnames(df_pt) <- c("TN_eqc", "TN_qc", "TX_eqc", "TX_qc")
 # 
 # 
 # plot(df_pt, type = "p", cex = .1, main = "")
 # 
 # dev.off()
 # 
 # }
  
  raw_temp_TN_5_6[[j]] <- tn_ts_eqc
  raw_temp_TX_5_6[[j]] <- tx_ts_eqc
  
  raw_spat_St_eqc$TX_eqc[j] <- all( is.na(tx_ts_eqc) )
  raw_spat_St_eqc$TN_eqc[j] <- all( is.na(tn_ts_eqc) )
  
}

####

raw_spat_St_eqc <- raw_spat_St_eqc %>%
  subset(TX_eqc == FALSE & TN_eqc == FALSE)

raw_spat_St_eqc %>%  plot(YY ~ XX, .)

####

names(raw_temp_TN_5_6) <- names(raw_temp_TN_5)
names(raw_temp_TX_5_6) <- names(raw_temp_TX_5)

tn_del <- sapply(raw_temp_TN_5_6, function(z) all(is.na(z)) ) %>% .[which(. == T)] %>% names()
tx_del <- sapply(raw_temp_TX_5_6, function(z) all(is.na(z)) ) %>% .[which(. == T)] %>% names()

stat_del <- c(tx_del, tn_del) %>% unique()

raw_temp_TN_5_6 <- raw_temp_TN_5_6[ -match(stat_del, names(raw_temp_TN_5_6) ) ]
raw_temp_TX_5_6 <- raw_temp_TX_5_6[ -match(stat_del, names(raw_temp_TX_5_6) ) ]

all(raw_spat_St_eqc$CC == names(raw_temp_TN_5_6))
all(raw_spat_St_eqc$CC == names(raw_temp_TX_5_6))

#### visual inspection 

#deleted

del_0 <-c("X171", "X177", "X327", "X358", "X363", "X372", "X433", 
          "X444", "X452", "X598", "X634", "X644", "X646", "X681", "X682", 
          "X706", "X733", "X737", "X738", "X744", 
          "X796", "X825", "X861", "X996", "X997", "X6672", "X105121", 
          "X140307", "X140434", "X150904", "X155107", "X156111", "X156131", 
          "X156306", "X156401", "X157310", "X158315",  "X0022GHCN", 
          "X170", "X173", "X275", "X380", "X828", "X874", "X5571", 
          "X117054", "X152102", "X152110", "X153225", "X153312", "X153319", 
          "X154303", 
          
          "X0005GHCN", "X0006GHCN","X0007GHCN","X0009GHCN","X0014GHCN","X0016GHCN",
          "X0021GHCN","X110","X150","X151","X152", "X178","X203","X209","X251","X256",
          "X275","X279","X297","X299","X300","X307","X326", "X327","X329","X338", 
          "X350","X355","X356","X358","X363", "X372", "X379","X380","X381","X390","X396", "X397", "X398",
          "X402","X412","X413","X427","X433","X436", "X444","X448","X449","X452","X453","X455", "X475", 
          "X489","X502","X532", "X535", "X538", "X544", "X545", "X579", "X598", "X602", "X634", "X641",
          "X644", "X646", "X652", "X681" , "X682", "X689", "X706", "X714","X715", "X725", "X732","X733",
          "X737","X738","X739","X744","X756", "X798", "X801", "X803", "X808", "X817", "X818", "X819", "X821",
          "X825", "X828", "X836", "X857", "X861", "X862", "X863", "X874", "X994", "X996", "X997", "X4431",
          "X5232", "X5570", "X5571", "X6637", "X6641", "X6670", "X6672", "X6680", "X8331", "X104097", 
          "X105121", "X109085", "X109090", "X110127", "X111174", "X113235", "X114108", "X114117", 
          "X116073", "X117054", "X140206", "X140306", "X140307", "X140434", "X140654", "X150900", 
          "X150901", "X150904", "X151503", "X151602", "X152102", "X152103", "X152110", "X152112", 
          "X152114", "X152129", "X152230", "X153103", "X153110", "X153114", "X153225", "X153312", 
          "X153314", "X153319", "X154100", "X154108", "X154303", "X155107", "X155121", "X155480", 
          "X156100", "X156102", "X156109", "X156110", "X156111", "X156121", "X156122", "X156130", 
          "X156131", "X156205", "X156217", "X156401", "X157307", "X157310", "X157312", "X157313",
          "X157317", "X157418", "X157419", "X158204", "X158308", "X158309", "X158313", "X158315",
          "X158317", "X158332") %>% unique()

####

raw_spat_St_eqc <- raw_spat_St_eqc %>% 
  .[-match(del_0, .$CC), ]

raw_temp_TN_5_6 <- raw_temp_TN_5_6 %>%
  .[raw_spat_St_eqc$CC] %>%
  do.call(cbind, .)
colnames(raw_temp_TN_5_6) <- raw_spat_St_eqc$CC

raw_temp_TX_5_6 <- raw_temp_TX_5_6 %>%
  .[raw_spat_St_eqc$CC] %>%
  do.call(cbind, .)
colnames(raw_temp_TX_5_6) <- raw_spat_St_eqc$CC

###

raw_temp_TN_5 <- raw_temp_TN_5 %>%
  .[raw_spat_St_eqc$CC] %>%
  do.call(cbind, .)
colnames(raw_temp_TN_5) <- raw_spat_St_eqc$CC

raw_temp_TX_5 <- raw_temp_TX_5 %>%
  .[raw_spat_St_eqc$CC] %>%
  do.call(cbind, .)
colnames(raw_temp_TX_5) <- raw_spat_St_eqc$CC

### saving data 

save(raw_temp_TN_5_6, raw_temp_TX_5_6, 
     raw_temp_TN_5, raw_temp_TX_5, 
     raw_spat_St_eqc,
     file = file.path("/media","buntu","D1AB-BCDE","databases","workflow_databases","step04_QCDATA_03.RData"))
