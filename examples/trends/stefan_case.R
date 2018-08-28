library(xts)
library(dplyr)
library(data.table)

rm(list = ls())

### source codes 

source('./functions/tools_get_os.R')

##############

if( get_os() == "windows" ) {
  
  load(file.path("G:","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step06_QCDATA_05.RData"))
  ls()
  
} else if ( get_os() == "linux") {
  
  load(file.path("/media","buntu","TOSHIBA EXT","DATABASES","DATA","TEMPERATURE_OBSDATASET","databases","step06_QCDATA_05.RData"))
  ls()
}

############

res_tx <- fdstats %>% 
  subset(DRE == "13") %>%
  select(CC) %>%
  unlist() %>%
  as.character() %>%
  HMdata_tx[, .] %>%
  apply(., 2, function(z){
    z <- xts(z, time(.))
    z_anl <- apply.yearly(z, function(y) mean(y))
    res <- mkTrend(as.vector(z_anl), ci = 0.95)$"Sen's Slope"
    res <- round(res/0.1, 2)
    return(res)
  }) 

res_tn <- fdstats %>% 
  subset(DRE == "13") %>%
  select(CC) %>%
  unlist() %>%
  as.character() %>%
  HMdata_tn[, .] %>%
  apply(., 2, function(z){
    z <- xts(z, time(.))
    z_anl <- apply.yearly(z, function(y) mean(y))
    res <- mkTrend(as.vector(z_anl), ci = 0.95)$"Sen's Slope"
    res <- round(res/0.1, 2)
    return(res)
  }) 


res_slope <- rbind(data.frame(subset(fdstats, DRE == "13"), slope = res_tx, var = "TX"),
                   data.frame(subset(fdstats, DRE == "13"), slope = res_tn, var = "TN"))

# res_slope <- rbind(data.frame(fdstats, slope = res_tx, var = "TX"),
#                    data.frame(fdstats, slope = res_tn, var = "TN"))


res_slope %>%
  mutate(var = factor(var)) %>%
  ggplot() + 
  geom_point(aes(x = XX,y = YY, fill = slope), shape = 21, size = 7) + 
  scale_fill_gradient2() +
  facet_wrap(~var)
