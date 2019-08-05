# Function: RK_errormet.R
# By: Sabine Loos
# Latest Update: 07.31.2019

# DESCRIPTION: Function used to quantify various errormetrics between two vectors, used to quantify errors of the framework and engineering forecast

# FUNCTIONS: 
## RK_errormet - quantifies error metrics of the prediction and uncertainty estimate

# INPUTS:
## newdata_shp = Spatial data frame with the predictions
## dependent.var_char = character value of the true dependent variable(e.g. "GT_OUT")
## pred.var_char = character value of the predicted variables to be compared to the true value (e.g. "RK_pred_fin")
## var.var_char = character value of the predicted varirance (e.g. "var_fin")
## ind_validation = vector of the index of newdata_shp used for validation
## error = T/F calculate error (e.g. predicted - observed)
## error_rel = T/F calculate relative error (e.g. (predicted-observed)/observed)
## error_sq = T/F calcuate the squared error (e.g. (predicted - observed)^2)
## error_abs = T/F calculate the absolute error (e.g. |predicted - observed|)
## SDSR = T/F calculate the standard deviation of the standardized residuals
## accplot = T/F calculate the accuracy plot and goodness metric G of the uncertainty
## return_shp = T/F return newdata_shp with calculated errormetrics

# OUTPUTS: output contains a list that contains:
## errormet =  dataframe with  chosen errormetrics as columns
## shp (optional) = newdata_shp with errors as columns


# RK_errormet -------------------------------------------------------------
RK_errormet <- function(newdata_shp, 
                        dependent.var_char = "GT_OUT",
                        pred.var_char = "RK_pred_fin",
                        var.var_char = "var_fin",
                        ind_validation,
                        error = T,
                        error_rel = T,
                        error_sq = T,
                        error_abs = T,
                        SDSR = T,
                        accplot = T, 
                        return_shp = F){
  # initialize dataframe to hole error metrics
  errormet <- data.frame(del = NA)
  # calculate the error: predicted - observed
  ERROR <- rowSums(cbind(newdata_shp@data[,pred.var_char], - newdata_shp@data[, dependent.var_char]), na.rm = F)
  
  # calculate error metrics
  if(error == T){ # error 
    newdata_shp$ERROR <- rowSums(cbind(newdata_shp@data[,pred.var_char], - newdata_shp@data[, dependent.var_char]), na.rm = F) # add column of error
    errormet$ME <- mean(newdata_shp@data$ERROR[ind_validation], na.rm = T) # mean error
    errormet$VE <- var(newdata_shp@data$ERROR[ind_validation], na.rm = T) # variance of the error
    
  }
  if(error_rel == T){ # relative error
    newdata_shp$ERROR_rel <- ERROR/newdata_shp@data[, dependent.var_char] # add column of relative error
    errormet$MSRE <- mean((newdata_shp@data$ERROR_rel[ind_validation])^2, na.rm = T) # mean squared relative error
    errormet$MRE <- mean(newdata_shp@data$ERROR_rel[ind_validation], na.rm = T) # mean relative error
    errormet$VRE <- var(newdata_shp@data$ERROR_rel[ind_validation], na.rm = T) # variance of the relative error
  }
  if(error_sq == T){
    newdata_shp$ERROR_sq <- ERROR^2 # add column of squared error
    errormet$MSE  <- mean(newdata_shp@data$ERROR_sq[ind_validation], na.rm = T) # mean squared error
  }
  if(error_abs == T){
    newdata_shp$ERROR_abs <- abs(ERROR) # add column of absolute error
    errormet$MAE <- mean(newdata_shp@data$ERROR_abs[ind_validation], na.rm = T) # mean absolute error
  }
  if(SDSR == T){
    errormet$SDSR <- sd(ERROR[ind_validation]/sqrt(newdata_shp@data[ind_validation, var.var_char]), na.rm = T) # # add column of standard deviation of standardized residual
  }
  if(accplot == T){ # accuracy plot
    # The accuracy plot looks at the accuracy of the uncertainty
    # more information can be found in:
    # Deutsch, C., 1997. Direct assessment of local accuracy and precision. In Baafi, E. and Schofield, N.
    # (eds.), GeostatisticsWollongong ’96, 1 edn., pp. 115–125. Kluwer Academic Publishers,Wollongong, Australia.
    p_RK = seq(0,100, by = 0.5)
    sd <- qnorm((0.5 + p_RK/200)); sd[length(p_RK)] <- qnorm(0.999999)
    psi_TRUE <- sapply(X = sd, function(x, mid = newdata_shp@data[,pred.var_char], 
                                               var_fin = newdata_shp@data[,var.var_char],
                                               GT_OUT = newdata_shp@data[, dependent.var_char]){
                                              sd = x
                                              plus_sd <- mid+sd*sqrt(var_fin)
                                              minus_sd <- mid-sd*sqrt(var_fin)
                                              within_sd <- ifelse(GT_OUT < plus_sd & GT_OUT > minus_sd, 1, 0)
                                              return(100*(sum(within_sd[ind_validation], na.rm = T)/nrow(newdata_shp)))})
    # calculate goodness metric, G
    errormet$G = 1 - sum(mapply(function(psi, p){
        diff <-  psi - p
        a <- ifelse(diff >= 0, 1, -2)
        sig <- a * diff
        return(sig)}, psi = psi_TRUE/100, p = p_RK/100)*(1/length(p_RK)))
  }
  # return outputs
  ifelse(return_shp==T,return(list(shp = newdata_shp, errormet = errormet[-which(names(errormet) %in% "del")])) ,return(errormet))
}
