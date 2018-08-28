
# Index of agreement (dr)
# based on Willmott et al (2012), code from ie2misc package
dr <- function (predicted, observed, na.rm = FALSE) {
  
  # The base::mean.default code has been helpful with regards to the treatment
  # of non-numeric values
  
  if (length(predicted) < 1 | length(observed) < 1) {
    
    stop("Check the length of the predicted and observed vectors since one of
         them has a length of 0. Use a different set of values and then try again.")
    # Source 1 / provide a stop warning if the length of one of the vectors is 0
    
  } else {
    
    if (length(predicted) != length(observed)) {
      
      stop("Check the length of the predicted and observed vectors since they don't
           match. Use a different set of values and then try again.")
      # Source 1 / provide a stop warning if the length of the numeric vectors do
      # not match
      
    } else {
      
      if (!is.numeric(predicted) | !is.numeric(observed)) {
        
        stop("Either the predicted or observed vector is not numeric. Use a
             different set of values and then try again.")
        # Source 1 / provide a stop warning if the either one of the vectors is not
        # numeric
        
      } else {
        
        c <- 2
        
        if (na.rm == TRUE) {
          
          if (sum(abs(predicted - observed), na.rm = na.rm) <=
              (c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm))) {
            
            1 - (sum(abs(predicted - observed), na.rm = na.rm) /
                   (c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm)))
            
          } else {
            
            ((c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm)) /
               sum(abs(predicted - observed), na.rm = na.rm)) - 1
            
          } }
        
        else {
          
          if (anyNA(predicted) | anyNA(observed)) {
            
            NA
            
          } else {
            
            if (sum(abs(predicted - observed), na.rm = na.rm) <=
                (c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm))) {
              
              1 - (sum(abs(predicted - observed), na.rm = na.rm) /
                     (c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm)))
              
            } else {
              
              ((c * sum(abs(observed - mean(observed, na.rm = na.rm)), na.rm = na.rm)) /
                 sum(abs(predicted - observed), na.rm = na.rm)) - 1
              
            } }
          
          
        }
      }
    }
  }
}
#
#set.seed(100) # makes the example reproducible
#obs1 <- rnorm(100) # observed
#pre1 <- rnorm(100) # predicted
#using the vectors pre1 and obs1
#dr(pre1, obs1)


# Mean-absolute error (MAE)
# based on Cort J. Willmott and Kenji Matsuura (2005), code from ie2misc package
mae <- function (predicted, observed, na.rm = FALSE) {
  
  # The base::mean.default code has been helpful with regards to the treatment
  # of non-numeric values
  
  # The moments::kurtosis code has been helpful with regards to the treatment of
  # na.rm
  
  if (length(predicted) < 1 | length(observed) < 1) {
    
    stop("Check the length of the predicted and observed vectors since one of
         them has a length of 0. Use a different set of values and then try again.")
    # Source 1 / provide a stop warning if the length of one of the vectors is 0
    
  } else {
    
    if (length(predicted) != length(observed)) {
      
      stop("Check the length of the predicted and observed vectors since they don't
           match. Use a different set of values and then try again.")
      # Source 1 / provide a stop warning if the length of the numeric vectors do not match
      
    } else {
      
      if (!is.numeric(predicted) | !is.numeric(observed)) {
        
        stop("Either the predicted or observed vector is not numeric. Use a different
             set of values and then try again.")
        # Source 1 / provide a stop warning if the either one of the vectors is not numeric
        
      } else {
        
        if (na.rm == TRUE) {
          
          observed <- observed[!is.na(observed)]
          
          predicted <- predicted[!is.na(predicted)]
          
          n <- length(predicted)
          
          (n ^ -1) * (sum(abs(predicted - observed), na.rm = na.rm))
          
        } else {
          
          n <- length(predicted)
          
          (n ^ -1) * (sum(abs(predicted - observed), na.rm = na.rm))
          
        }
      }
    }
  }
}
#
#obs <- 1:10 # observed
#pre <- 2:11 # predicted
#mae(pre, obs)

# 'obs'   : numeric 'data.frame', 'matrix' or 'vector' with observed values
# 'sim'   : numeric 'data.frame', 'matrix' or 'vector' with simulated values
# 'Result': Percent Bias between 'sim' and 'obs', 
#           when multiplied by 100, its units is percentage
# Ref: Yapo P. O., Gupta H. V., Sorooshian S., 1996. 
#      Automatic calibration of conceptual rainfall-runoff models: 
#      sensitivity to calibration data. Journal of Hydrology. v181 i1-4. 23-48.
pbias <-function(sim, obs, ...) UseMethod("pbias")

pbias.default <- function (sim, obs, na.rm=TRUE, ...){
  
  if ( is.na(match(class(sim), c("integer", "numeric", "ts", "zoo"))) |
       is.na(match(class(obs), c("integer", "numeric", "ts", "zoo")))
  ) stop("Invalid argument type: 'sim' & 'obs' have to be of class: c('integer', 'numeric', 'ts', 'zoo')")
  
  # index of those elements that are present both in 'x' and 'y' (NON- NA values)
  vi <- valindex(sim, obs)
  
  # Filtering 'obs' and 'sim', selecting only those pairs of elements 
  # that are present both in 'x' and 'y' (NON- NA values)
  obs <- obs[vi]
  sim <- sim[vi]
  
  # lenght of the data sets that will be ocnsidered for the ocmputations
  n <- length(obs)
  
  denominator <- sum( obs )
  
  if (denominator != 0) {
    
    pbias <-  mean( sim - obs )
    
  } else {
    pbias <- NA
    warning("'sum((obs)=0', it is not possible to compute 'pbias'")  
  } # ELSE end
  
  return( round(pbias, 3) )
  
} # 'pbias.default' end


pbias.matrix <- function (sim, obs, na.rm=TRUE, ...){
  
  # Checking that 'sim' and 'obs' have the same dimensions
  if ( all.equal(dim(sim), dim(obs)) != TRUE )
    stop( paste("Invalid argument: dim(sim) != dim(obs) ( [", 
                paste(dim(sim), collapse=" "), "] != [", 
                paste(dim(obs), collapse=" "), "] )", sep="") )
  
  pbias <- rep(NA, ncol(obs))       
  
  pbias <- sapply(1:ncol(obs), function(i,x,y) { 
    pbias[i] <- pbias.default( x[,i], y[,i], na.rm=na.rm, ... )
  }, x=sim, y=obs )        
  
  return(pbias)
  
} # 'pbias.matrix' end


pbias.data.frame <- function (sim, obs, na.rm=TRUE, ...){ 
  
  sim <- as.matrix(sim)
  obs <- as.matrix(obs)
  
  pbias.matrix(sim, obs, na.rm=na.rm, ...)
  
} # 'pbias.data.frame' end  


################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 22-Mar-2013                                                         #
# Updates:                                                                     #
################################################################################
pbias.zoo <- function(sim, obs, na.rm=TRUE, ...){
  
  sim <- zoo::coredata(sim)
  if (is.zoo(obs)) obs <- zoo::coredata(obs)
  
  if (is.matrix(sim) | is.data.frame(sim)) {
    pbias.matrix(sim, obs, na.rm=na.rm, ...)
  } else NextMethod(sim, obs, na.rm=na.rm, ...)
  
} # 'pbias.zoo' end

