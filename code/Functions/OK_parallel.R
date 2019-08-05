# Function: OK_parallel.R
# By: Sabine Loos
# Latest Update: 07.31.2019

# DESCRIPTION: Function used to perform kriging with the option of doing it in a parallel process (really only a space saver if wanting to do parallel)
#              Parallel processing requires a parallel backend (specifically foreach and future)

# FUNCTIONS: 
## OK_parallel - perform kriging with input data frame and predict at new locations. 

# INPUTS:
## vgm.model = variogram model fit by gstat or autofit
## locn_shp = Spatial data frame with the locations of the training data (i.e. the field data)
## newdata_shp = Spatial data frame with the locations where you would like to predict the trend at (i.e. the full area of interest)
## maxdist.multiplier = used to perform local kriging (and speed up analysis). Multiply this number by the range of the variogram
## force.option = T/F to indicate what version of local kriging used (see gstat manual)
## nmin = minimum number of points to consider  with local kriging (see gstat manual)
## nmax = maximum number of points to consider  with local kriging (see gstat manual)
## dependent.var = character value of variable we're building the variogram on (e.g. "trnd_resid_OLS")
## devuglev = level of verboseness (see gstat manual)
## parallel = T/F indicator of whether to use parallel kriging
## n_cores = number of cores to use with parallel processing

# OUTPUTS: parallelKrige which contains
## var1.pred = Ordinary kriging prediction of the dependent variable
## var1.var = Ordinary kriging prediction of the dependent variable

# EXAMPLE:
# registerDoFuture()
# plan(list(multiprocess))
# test <- OK_parallel(resid.vgm, field_sample_shp,
#                     pred_grid_shp, maxdist.multiplier = 2, 
#                     nmin = 2, nmax = 20, dependent.var = "trnd_resid_GLS", n_cores = 6)

# krige residuals in parallel, note must have parallel backend loaded for foreach to work
OK_parallel <- function(vgm.model, locn_shp, newdata_shp, maxdist.multiplier = 5, force.option = T, 
                        nmin = 2, nmax = 20, dependent.var, debuglev = -1, parallel = T, n_cores = NA){
  require(gstat); require(foreach); require(future); require(doFuture)
  # formula for the variogram
  form <- as.formula(paste(dependent.var,  "~ 1"))
  # range used to specify the max distance for local kriging
  alpha.range = diff(vgm.model$var_model$range)
  
  if(parallel == T){
    # split newdata shape into n_cores subsections (we split this way for merging predictions back together)
    parts <- split(x = 1:length(newdata_shp), ceiling(seq_along(1:length(newdata_shp))/(length(newdata_shp)/n_cores)))
    # print(paste("Kriging on", n_cores, "subsections of newdata_shp"))
    # print(paste("OK PID =", Sys.getpid())) # what core are yyou on?
    # krige each subsection in parallel
    parallelKrige <- foreach(X = 1:length(parts), 
                             .packages = c("gstat","future"),
                             .export = c("locn_shp","newdata_shp","alpha.range",
                                                              "vgm.model", "parts", "maxdist.multiplier", "force.option",
                                                              "nmin", "nmax", "form", "debuglev", "n_cores")) %dopar% {
                                                                gstat::krige(formula =  form, # ordinary kriging
                                                                             locations = locn_shp, # locations of field sampled data
                                                                             newdata = newdata_shp[parts[[X]],], # new locations to predict on
                                                                             maxdist = alpha.range*maxdist.multiplier, # specify maximum distance to speed up the kriging process
                                                                             nmin = nmin,
                                                                             nmax = nmax,
                                                                             debug.level = debuglev, #prints all errors
                                                                             force = force.option, # forces neighbourhood selection (look further than maxdist when nmin isn't reached)
                                                                             model = vgm.model$var_model)  # residual variogram
                                                              }
    
    # Merge all the predictions back together
    parallelKrige <- do.call("rbind", parallelKrige)
  }else{ # do it for the entire area without parallel 
    parallelKrige <- gstat::krige(formula =  form, # ordinary kriging
                 locations = locn_shp, # locations of field sampled data
                 newdata = newdata_shp, # new locations to predict on
                 maxdist = alpha.range*maxdist.multiplier, # specify maximum distance to speed up the kriging process
                 nmin = nmin,
                 nmax = nmax,
                 debug.level = debuglev, #prints all errors
                 force = force.option, # forces neighbourhood selection (look further than maxdist when nmin isn't reached)
                 model = vgm.model$var_model)  # residual variogram
  }

  return(parallelKrige)
}



