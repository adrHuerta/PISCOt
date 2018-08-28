library(xts)
library(ggplot2)
library(ggrepel)
library(raster)
library(sp)
library(dplyr)

rm(list = ls())

#############

load("./data/shp_dataset.RData")

if( get_os() == "windows" ) {
  
  load(file.path("/G:", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","observed_dataset.RData") )
  load(file.path("/G:", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","gridded_dataset.RData") )
  ls()
  
} else if ( get_os() == "linux") {
  
  load(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","observed_dataset.RData") )
  load(file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","gridded_dataset.RData") )
  ls()
}


#############

ggplot() + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "skyblue", colour = "skyblue") +
  geom_point(data = xy_stat, 
             aes( x = XX, y = YY), 
             size = 1, colour = "blue") + 
  geom_text_repel(data = xy_stat, 
            aes( x = XX, y = YY, label = CC),
            size = 2.5, colour = "black", fontface = 'bold') +
  coord_quickmap(ylim = c(-17.5, -11),
                 xlim = c(-75, -68.5)) + theme_bw() + xlab("") + ylab("")

#############

data_tn %>%
  apply(1, function(x) sum(!is.na(x))) %>% 
  xts(seq(as.Date("1981-01-01"), as.Date("2015-12-31"), by = "day")) %>%
  fortify.zoo() %>% 
  ggplot() + 
  geom_line(aes(y = ., x = Index), colour = "red") + 
  geom_line(data = data_tx %>%
              apply(1, function(x) sum(!is.na(x))) %>% 
              xts(seq(as.Date("1981-01-01"), as.Date("2015-12-31"), by = "day")) %>%
              fortify.zoo(), 
            aes(y = ., x = Index), colour = "blue")  +
  scale_y_continuous(limits = c(20, 32), breaks = seq(20, 32, 2), 
                     expand = c(0,0)) +
  scale_x_date(date_breaks = "5 years", limits = c(as.Date("1980-01-01"), as.Date("2016-01-01")), 
               date_labels = "%Y" , expand = c(0,0)) + 
  theme_bw() + xlab("") + ylab("") 

#############


lst_data_monthly_min[[1]] %>% 
  as.data.frame(xy = T) %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = X01)) + 
  scale_fill_gradientn(limits = c(-10.5, 25), 
                       breaks = c(-10.5, -5, 0, 5, 10, 15, 20, 25),
                       colours = topo.colors(10)) + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "gray20", colour = "gray20") +
  coord_quickmap(ylim = c(-17.5, -12.5),
                 xlim = c(-74.5, -68.5)) + theme_bw() + xlab("") + ylab("") + 
  ggtitle("MODIS-LST noche: Promedio Enero (°C) \n(2000-2015)")

  

t2mn_era[[1]] %>% 
  as.data.frame(xy = T) %>% 
  ggplot() + 
  geom_tile(aes(x = x, y = y, fill = X732)) + 
  scale_fill_gradientn(limits = c(-10.5, 25),
                       breaks = c(-10.5, -5, 0, 5, 10, 15, 20, 25),
                       colours = topo.colors(10)) + 
  geom_polygon(data = shp_tlb, aes(x = long, y = lat, group = group), fill = NA, colour = "black") +
  geom_polygon(data = shp_lk, aes(x = long, y = lat, group = group), fill = "gray20", colour = "gray20") +
  coord_quickmap(ylim = c(-17.5, -12.5),
                 xlim = c(-74.5, -68.5)) + theme_bw() + xlab("") + ylab("") + 
  ggtitle("ERA-Interim : T2m (°C) \n01-01-1981")


#############

xy_stat_xy <- xy_stat %>%
  select(XX, YY, CC)
coordinates(xy_stat_xy) <- ~XX+YY
crs(xy_stat_xy) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 


### ERA

t2mx_era_xy <- extract(t2mx_era, xy_stat_xy)
t2mn_era_xy <- extract(t2mn_era, xy_stat_xy)

lst_data_monthly_min_xy <- extract(brick(lst_data_monthly_min), xy_stat_xy)
lst_data_monthly_max_xy <- extract(brick(lst_data_monthly_max), xy_stat_xy)

lst_data_pentadal_max_xy <- extract(lst_data_pentadal$max, xy_stat_xy)
lst_data_pentadal_min_xy <- extract(lst_data_pentadal$min, xy_stat_xy)

##### 
t2mx_era_xy_r <- t2mx_era_xy %>% 
  t() %>%
  xts(time(data_tx))

colnames(t2mx_era_xy_r) <- colnames(data_tx)


t2mn_era_xy_r <- t2mn_era_xy %>% 
  t() %>%
  xts(time(data_tn))

colnames(t2mn_era_xy_r) <- colnames(data_tn)


lst_data_monthly_min_xy_r <- lst_data_monthly_min_xy %>%
  t() 
colnames(lst_data_monthly_min_xy_r) <- colnames(data_tn)

lst_data_monthly_max_xy_r <- lst_data_monthly_max_xy %>%
  t() 
colnames(lst_data_monthly_max_xy_r) <- colnames(data_tx)

lst_data_pentadal_max_xy_r <- lst_data_pentadal_max_xy %>%
  t() 
colnames(lst_data_pentadal_max_xy_r) <- colnames(data_tx)

lst_data_pentadal_min_xy_r <- lst_data_pentadal_min_xy %>%
  t()
colnames(lst_data_pentadal_min_xy_r) <- colnames(data_tn)


#####
 
t2mx <- t2mx_era_xy_r
t2mn <- t2mn_era_xy_r
lst_m_mn <- lst_data_monthly_min_xy_r
lst_m_mx <- lst_data_monthly_max_xy_r
lst_p_mx <- lst_data_pentadal_max_xy_r
lst_p_mn <- lst_data_pentadal_min_xy_r


############

if( get_os() == "windows" ) {
  
  
  save(lst_data_monthly_max, 
       lst_data_monthly_min,
       lst_data_pentadal,
       t2mx_era, 
       t2mn_era,
       file = file.path("/G:", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","all_gridded_dataset.RData"))
  
  
} else if ( get_os() == "linux") {
  
  
  save(lst_data_monthly_max, 
       lst_data_monthly_min,
       lst_data_pentadal,
       t2mx_era, 
       t2mn_era,
       file = file.path("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "PISCO_Temp_examples", "validation","all_gridded_dataset.RData"))
  
}
