rm(list = ls())

library(raster)
library(sp)
library(dplyr)
library(ncdf4)
library(xts)

### source codes 

source('./functions/tools_get_os.R')

############

if( get_os() == "windows" ) {
  
  lst_data_pentadal <- dir(file.path("G:", "DATABASES", "DATA", "LST", "PENTADAL"),
                          full.names = T)
  
  lst_data_monthly_max <- dir(file.path("G:", "DATABASES", "DATA", "LST", "MENSUAL","max"),
                              full.names = T)
  lst_data_monthly_min <- dir(file.path("G:", "DATABASES", "DATA", "LST", "MENSUAL","min"),
                              full.names = T)
  
  t2mx_era <- dir(file.path("G:", "DATABASES", "DATA", "T2MAX_ERA_INTERIM", "validation"),
                              full.names = T)
  t2mn_era <- dir(file.path("G:", "DATABASES", "DATA", "T2MIN_ERA_INTERIM", "validation"),
                  full.names = T)
  

} else if ( get_os() == "linux") {
  
  lst_data_pentadal <- dir(file.path("/media","buntu","TOSHIBA EXT","DATABASES", "DATA", "LST", "PENTADAL"),
                           full.names = T)
  
  lst_data_monthly_max <- dir(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "LST", "MENSUAL","max"),
                              full.names = T)
  lst_data_monthly_min <- dir(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "LST", "MENSUAL","min"),
                              full.names = T)
  
  t2mx_era <- dir(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "T2MAX_ERA_INTERIM", "validation"),
                  full.names = T)
  t2mn_era <- dir(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "T2MIN_ERA_INTERIM", "validation"),
                  full.names = T)
}

#####

study_area <- extent(-75, -68, -18, -11)

lst_data_pentadal <- lst_data_pentadal %>%
  as.list() %>%
  lapply(., function(x){
    res <- brick(x) - 273.15
    crs(res) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
    return(crop(res, study_area))
  } )
  
lst_data_monthly_max <- lst_data_monthly_max %>%
  as.list() %>%
  lapply(., function(x){
    res <- raster(x)
    crs(res) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
    return(crop(res, study_area))
  } )

lst_data_monthly_min <- lst_data_monthly_min %>%
  as.list() %>%
  lapply(., function(x){
    res <- raster(x)
    crs(res) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
    return(crop(res, study_area))
  } )

names(lst_data_monthly_max) <- paste("X", 1:12, sep = "")
names(lst_data_monthly_min) <- paste("X", 1:12, sep = "")
names(lst_data_pentadal) <- c("max", "min")

################ 

time_seq_era <- xts(1:length(seq(as.Date("1979-01-01"), as.Date("2016-08-01"), by = "day")), 
                    seq(as.Date("1979-01-01"), as.Date("2016-08-01"), by = "day")) %>%
  window(start = "1981-01-01", end = "2015-12-31") %>%
  coredata()

t2mx_era <- t2mx_era %>% 
  brick() %>%
  .[[time_seq_era]]

t2mn_era <- t2mn_era %>% 
  brick() %>%
  .[[time_seq_era]]


plot(lst_data_pentadal$max[[1]])
plot(lst_data_monthly_min[[1]])
plot(t2mx_era[[1]])

############

if( get_os() == "windows" ) {
  
  
  
} else if ( get_os() == "linux") {
  
  
}

#####

############

if( get_os() == "windows" ) {
  
  
  save(lst_data_monthly_max, 
       lst_data_monthly_min,
       lst_data_pentadal,
       t2mx_era, 
       t2mn_era,
       file = file.path("/G:", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","gridded_dataset.RData"))
  
  
} else if ( get_os() == "linux") {
  
  
  save(lst_data_monthly_max, 
       lst_data_monthly_min,
       lst_data_pentadal,
       t2mx_era, 
       t2mn_era,
       file = file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","gridded_dataset.RData"))
  
}

#####