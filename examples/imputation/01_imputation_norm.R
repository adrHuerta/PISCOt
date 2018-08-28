#following Oyler's code [Oyler et al (2014)]
library(norm)
rm(list = ls())

### source codes 

source('./functions/tools_get_os.R')

###########

if( get_os() == "windows" ) {
  
  load("/G:", "DATABASES", "DATA", "PISCO_Temp_examples", "imputation", "data_tx.RData")
  ls()
  
} else if ( get_os() == "linux") {
  
  load("/media","buntu","TOSHIBA EXT", "DATABASES", "DATA", "PISCO_Temp_examples", "imputation", "data_tx.RData")
  ls()
}




mat <- data_tx

rngseed(4324)
mat[is.nan(mat)]<-NA
pre <- prelim.norm(mat)
mle <- em.norm(pre,showits=FALSE)

impute.params <- getparam.norm(pre,mle)
mu <- impute.params$mu[1]
sigma <- impute.params$sigma[1,1]

mle.imputed <- imp.norm(pre, mle, mat)
