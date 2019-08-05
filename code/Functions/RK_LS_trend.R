# Function: RK_LS_trend.R
# By: Sabine Loos
# Latest Update: 07.31.2019

# DESCRIPTION: Function used to develop trend model in the framework. Here we test ordinary least squares (OLS) and generalized least squares(GLS)
#              using 5-fold cross-validation.
#              GLS is similar to OLS, except that points are weighted by the spatial covariance matrix (to account for spatial clustering)
#              More information on each can be found in the paper. 

# FUNCTIONS: 
## RK_LS_trend - Stands for regression kriging least squares trend. Develops trend model for given training locations and returns prediction at new locations

# INPUTS:
## locn_shp = Spatial data frame with the locations of the training data (i.e. the field data)
## newdata_shp = Spatial data frame with the locations where you would like to predict the trend at (i.e. the full area of interest)
## stepwise = T/F for whether to perform stepwise selection
## dependent.var_char = character value of the dependent variable for the trend, m (e.g. "GT_OUT")
## independent.var_vec = character vector of the independent variables to considered in the trend, X (e.g. c("DPM_bldg_med", "MMI_mn", "ENG_mnDR", "DEM_mn"))
## id.var_char = character value of the variable that indicates the ID for each location, s (e.g. "GRID_ID")
## return_se = T/F for whether to return the standard errors on the coefficients

# OUTPUTS: output contains a list that contains:
## locn_shp = original locn_shp spatial data frame with added trend variables
  ## trnd_OLS = prediction using OLS
  ## trnd_resid_OLS = residuals from prediction using OLS
  ## trnd_GLS = prediction using GLS
  ## trnd_resid_GLS = residuals from prediction using GLS
## newdata_shp = original newdata_shp spatial data frame with added trend variables (same as above)
## vgm = the fitted variogram for GLS model (see autofit package for description of these variables)
## RMSE = dataframe with RMSE from different trend models (used to select best trend model)
## se (optional) = list of standard errors
  ## ols - standard errors of coefficient estimates from ordinary least squares
  ## gls - standard errors of coefficient estimates from generalized least squares
  ## ols.coeff - coefficient values from ordinary least squares estimate
  ## gls.coeff - coefficient values from generalized least squares estimate

# RK_LS_trend function ----------------------------------------------------
RK_LS_trend <- function(locn_shp = field_sample_shp, 
                        newdata_shp = pred_grid_shp,
                        stepwise = F,
                        dependent.var_char,
                        independent.var_vec,
                        id.var_char,
                        return_se = F){
  # required packages
  require(automap); require(gstat); require(caret)
  
  # define the parameters of a 5-fold cross-validation
  n_folds <- 5
  train.control <- caret::trainControl(method = "cv", number = n_folds, allowParallel = F)
  
  # Develop the initial OLS trend -------------------------------------------
    # Create formula with dependent and independent variables
    form <- as.formula(paste(dependent.var_char, paste(independent.var_vec, sep = "", collapse = " + "), sep = " ~ "))
    # set seed for reproducibility
    set.seed(212) 
    # develop linear OLS models using stepwise selection (for multicollinearity) or all independent variables
    if(stepwise == T){
      # Here we use mixed stepwise selection for OLS regression
      # do mixed stepwise selection using 5-fold cross-validation to choose the best model with best subset of predictors
      # Train the model
      lm.OLS <- caret::train(form, data = locn_shp@data,
                          method = "leapSeq", # can also be leapForward (for forward selection) and leapBackward (for backward selection) 
                          tuneGrid = data.frame(nvmax = 1:length(independent.var_vec)), #  the number of variable in the model
                          trControl = train.control)
      beta_OLS <- coef(lm.OLS$finalModel, id = lm.OLS$bestTune$nvmax)# save the coefficients
      lm_terms <- names(beta_OLS)[-1] # terms kept in the OLS model
      newdata_shp$trnd_OLS <- as.vector(as.matrix(cbind(int = rep(1, nrow(newdata_shp)),newdata_shp@data[,lm_terms])) %*% beta_OLS) # predicting at new locations
      RMSE_OLS <- mean(lm.OLS$resample$RMSE) # what is the average RMSE of all 5 folds? use for model selection
    }else{# otherwise no mixed stepwise selection
      lm.OLS <- caret::train(form, data = locn_shp@data,
                             method = "lm", # no stepwise selection
                             trControl = train.control)
      beta_OLS <- coef(lm.OLS$finalModel) # save the coefficients
      lm_terms <- names(beta_OLS)[-1] # terms in the OLS model (this should be the same as dependent.vars)
      newdata_shp$trnd_OLS <- as.vector(as.matrix(cbind(int = rep(1, nrow(newdata_shp)),newdata_shp@data[,lm_terms])) %*% beta_OLS) # predicting at new locations
      RMSE_OLS <- mean(lm.OLS$resample$RMSE) # what is the average RMSE of all 5 folds? use for model selection
    }
  # Calculate spatial covariance of residuals -------------------------------------------
    # Now that we have an initial trend model, we can calculate the spatial covariance of the residuals at the training locations
    # Calculate residuals
    locn_shp <- sp::merge(locn_shp, newdata_shp@data[,c(id.var_char, "trnd_OLS")], by = id.var_char, all.x = T) # merge prediction at new with training locations
    locn_shp$trnd_resid_OLS <- locn_shp@data[,dependent.var_char] - locn_shp$trnd_OLS # calculate the residuals (difference between truth and prediction)
    # Fit variogram to residuals using autofit
    resid.vgm <-automap::autofitVariogram(formula = trnd_resid_OLS~1, # variogram of only the residuals
                                input_data = locn_shp, # using the locations of the training data
                                model = c("Exp", "Sph", "Mat"), # considering exponential, spherical, and matern variograms
                                verbose = F) # suppress outputs
    # Use outputs of variogram to develop covariance matrix, C
    # Create distance matrix of the training locations
    dist_ctrs <- as.matrix(dist(x = cbind(lng = coordinates(locn_shp)[,1],
                                          lat = coordinates(locn_shp)[,2]),
                                diag = T, upper = T,
                                method = "euclidean"))
    # Compute covariance matrix of residuals (n x n)
    gamma <- gstat::variogramLine(resid.vgm$var_model, dist_vector = dist_ctrs)
    C <- gstat::variogramLine(resid.vgm$var_model, dist_vector = dist_ctrs, covariance = T)
    # check if C is singular (determinant equals 0)
    if(abs(0 - det(C)) < 5e-5){diag(C) <- 1} # replace with 1 if true
    
  # Develop GLS trend using covariance matrix, C -------------------------------------------
    # Compute matrix q (n x p) with values of predictors at each location (plus vector of 1's)
    n = nrow(locn_shp) # n = number of locations in training data
    q <- as.matrix(cbind(int = rep(1, n),locn_shp@data[,lm_terms])) # predictor matrix at training locations
    # z is vector of the target variable (output)
    z <- locn_shp@data[,dependent.var_char]
    
    # calculate beta coefficients
    folds <- caret::createFolds(z, k = n_folds, list = F, returnTrain = F)
    RMSE <- rep(NA,n_folds)
    # manually calculate beta coefficients of GLS (beta_gls <- solve(t(q) %*% solve(C) %*% q) %*% t(q) %*% solve(C) %*% (z))
    # use try here, sometimes result is insolvable because C is singular. If so, we don't use the GLS trend
    for (j in 1:n_folds) {
      beta_gls <- try(solve(t(q[folds != j,]) %*% solve(C[folds != j,folds != j]) %*% q[folds != j,]) %*% t(q[folds != j,]) %*% solve(C[folds != j,folds != j]) %*% (z[folds != j]))
      pred <- q[folds == j,] %*% beta_gls
      RMSE[j] <- sqrt(mean((pred - z[folds ==j])^2))
    }
    # coefficients are from the fold with the least RMSE
    j <- which.min(RMSE)
    beta_gls <- try(solve(t(q[folds != j,]) %*% solve(C[folds != j,folds != j]) %*% q[folds != j,]) %*% t(q[folds != j,]) %*% solve(C[folds != j,folds != j]) %*% (z[folds != j]))
    RMSE_GLS = mean(RMSE, na.rm = T) # what is the average RMSE of all 5 folds? use for model selection
    # predict GLS drift at all locations
    newdata_shp$trnd_GLS <- as.matrix(cbind(int = rep(1, nrow(newdata_shp)),newdata_shp@data[,lm_terms])) %*% beta_gls
  
  # Calculate trend estimation variance -------------------------------------------
    # Recalculate residuals using GLS trend 
    locn_shp <- sp::merge(locn_shp, newdata_shp@data[,c(id.var_char, "trnd_GLS")], by = id.var_char, all.x = T) # merge prediction at new with training locations
    locn_shp$trnd_resid_GLS <- locn_shp@data[,dependent.var_char] - locn_shp$trnd_GLS # calculate the residuals (difference between truth and prediction)
    
    # Estimation variance calculation for OLS and GLS
    n0 = nrow(newdata_shp) # number of new locations
    q0 = t(cbind(int = rep(1, n0), newdata_shp@data[,lm_terms])) # predictor matrix at new locations
    newdata_shp$trnd_GLS_var <- colSums(t(t(q0) %*% solve(t(q) %*% solve(C) %*% q)) * q0) # GLS trend variance
    newdata_shp$trnd_OLS_var <- colSums(t(t(q0) %*% solve(t(q) %*% q)) * q0) # OLS trend variance
  
  # Saving outputs -------------------------------------------
    # dataframe of RMSE's
    RMSE = data.frame(model = c("trnd_OLS", "trnd_GLS"), RMSE = c(RMSE_OLS, RMSE_GLS))
    # if you want to return the standard errors
    if(return_se == T){
      # calculate standard errors of OLS
      X <- as.matrix(cbind(int = rep(1, nrow(locn_shp@data)),locn_shp@data[,lm_terms]))
      fitted.values <- X %*% beta_OLS
      res <- locn_shp@data[,dependent.var_char] - fitted.values
      df <- length(is.na(locn_shp@data[,dependent.var_char])) - length(lm_terms) - 1 # n - p - 1 with intercept
      sig <- sum(res^2)/df
      se.OLS <- sqrt(diag(solve(t(X) %*% X)) * sig)
      # calculate standard errors of GLS
      fitted.values <- X %*% beta_gls
      res <- locn_shp@data[,dependent.var_char] - fitted.values
      sig <- sum(res^2)/df
      se.GLS <- sqrt(diag(solve(t(X) %*% solve(C) %*% X)) * sig)
      # return gamma, C, and shapes
      return(list(locn_shp = locn_shp, newdata_shp = newdata_shp, vgm = resid.vgm, RMSE=RMSE, se = list(ols = se.OLS,gls= se.GLS, ols.coeff = beta_OLS, gls.coeff = beta_gls)))
      
    }else{
      # return gamma, C, and shapes
      return(list(locn_shp = locn_shp, newdata_shp = newdata_shp, vgm = resid.vgm, RMSE=RMSE))
    }
}
