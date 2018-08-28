all_to_netcdf <- function(files_dir, var_name)
  {
  
  files_dir <- as.list( list.files(files_dir, full.names = T) )
  
  lapply(files_dir, function(z) {
    
    netcdf_names <- paste(gsub('.{3}$', '', z), "nc", sep = "")
    
    raster::writeRaster(raster::raster(z), 
                        filename = netcdf_names,
                        format = "CDF",
                        overwrite = TRUE, 
                        varname = var_name)
    
  })
  
 }

matchTime_netcdf_raster <- function(files_dir, 
                                    time_netcdf = seq(as.Date("1981-01-01"), as.Date("2015-12-31"), by = "day"),
                                    pattern_netcdf = list(yy = c(4, 7),
                                                          mm = c(9, 10),
                                                          dd = c(12, 13)))
  {
  
  files_dir <- list.files(files_dir,  pattern = ".nc")
  dates_files_dir <- data.frame(files_dir, time_files_dir = ISOdate(substr(files_dir, pattern_netcdf$yy[1],  pattern_netcdf$yy[2]), 
                                                                    substr(files_dir, pattern_netcdf$mm[1],  pattern_netcdf$mm[2]),
                                                                    substr(files_dir, pattern_netcdf$dd[1],  pattern_netcdf$dd[2])) )
  dates_files_dir$time_files_dir <- as.Date(format(dates_files_dir$time_files_dir, "%Y-%m-%d"))
  
  if( !all(time_netcdf == dates_files_dir$time_files_dir) ) {
    stop("netcdf time doesnt match with dates")
  }
 }

merge_netcdf <- function(files_dir, 
                         time_netcdf = seq(as.Date("1981-01-01"), as.Date("2015-12-31"), by = "day"),
                         var_name)
  {
  
  system( paste("cd", files_dir, sep = " ") )
  
  matchTime_netcdf_raster(files_dir = files_dir, time_netcdf = time_netcdf)
  
  files_netcdf <- data.frame(files_dir = list.files(files_dir,  pattern = ".nc"), 
                             time_netcdf = substr(as.character(time_netcdf), 1, 4) )
  files_netcdf$time_netcdf <- factor(files_netcdf$time_netcdf)

  by(files_netcdf, files_netcdf$time_netcdf, function(z){
    
    nc_input <- paste( as.character(z$files_dir), sep = "", collapse = " ")
    nc_ouput <- paste( var_name, "_",z$time_netcdf[1], ".nc", sep = "")
    system_string <- paste("cdo", "cat", nc_input, nc_ouput, sep = " ")
    system(system_string)
    
  })
  
  nc_input <- paste( var_name, "_",levels(files_netcdf$time_netcdf), ".nc", sep = "", collapse = " ")
  nc_ouput <- paste( var_name, "_", 
                     levels(files_netcdf$time_netcdf)[1],
                     levels(files_netcdf$time_netcdf)[length(levels(files_netcdf$time_netcdf))], ".nc", sep = "")
  
  system_string <- paste("cdo", "cat", nc_input, nc_ouput, sep = " ")
  system(system_string)
  
  nc_ouput2 <- paste( var_name, "_", 
                     levels(files_netcdf$time_netcdf)[1], "_",
                     levels(files_netcdf$time_netcdf)[length(levels(files_netcdf$time_netcdf))], ".nc", sep = "")
  
  time_nc_code <- paste("-settaxis,", time_netcdf[1], ",12:00,", "1day", sep = "")
  system_string <- paste("cdo",time_nc_code , nc_ouput, nc_ouput2, sep = " ")
  system(system_string)
  
  system(paste("rm", nc_input, nc_ouput, sep = " "))
 }

files_dir = "/media/buntu/D1AB-BCDE/daily_values/tx"
var_name = "tx"
merge_netcdf(files_dir = files_dir,
             var_name = var_name)
