
stats_1 <- raw_spat_St_nearest[["X113162"]]
stats_1 <- raw_spat_St_nearest["X322"]

raw_temp_TN_4[as.character(stats_1$NN)] %>%
  do.call(cbind, .) %>%
  zoo() %>%
  plot(cex = 0.1, type = "p")

raw_dat_St_nearest_TN_4_NoNA_filled <- lapply(stats_1, function(x) {
  
  stats_n <- x$NN
  data_ts_merge <- raw_temp_TN_4[as.character(stats_n)]
  data_ts_merge <- do.call("cbind", data_ts_merge)
  colnames(data_ts_merge) <- as.character(stats_n)
  return(data_ts_merge)
  
}) %>%  
  lapply(., function(x){ make_data_spqc(x) }) %>% 
  lapply(., function(x){ filling_data_spqc(data_base = x) })



lapply(raw_dat_St_nearest_TN_4_NoNA_filled, function(x) spatial_qc(data_base = x))

data_base <- raw_dat_St_nearest_TN_4_NoNA_filled[[1]]


quantile(raw_dat_St_nearest_TN_4_NoNA_filled[[1]][,1], 0.25) + IQR(raw_dat_St_nearest_TN_4_NoNA_filled[[1]][,1])*3

plot(raw_dat_St_nearest_TN_4_NoNA_filled[[1]][,1], type = "p", cex = .1)
abline(h = quantile(raw_dat_St_nearest_TN_4_NoNA_filled[[1]][,1], 0.25) - IQR(raw_dat_St_nearest_TN_4_NoNA_filled[[1]][,1])*4)
abline(h = quantile(raw_dat_St_nearest_TN_4_NoNA_filled[[1]][,1], 0.75) + IQR(raw_dat_St_nearest_TN_4_NoNA_filled[[1]][,1])*4)

