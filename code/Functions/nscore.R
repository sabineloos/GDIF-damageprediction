# Function: nscore.R
# By: Sabine Loos
# Latest Update: 07.31.2019

# DESCRIPTION: Function used to perform a normal score transformation by matching quantiles

# FUNCTIONS: 
## nscore - normal score transformation function

# INPUTS:
## x = vector of values to be transformed using normal score transformation

# OUTPUTS: 
## nscore = vector of transformed values

# VERSION HISTORY:
## updated Sabine Loos, July 2019
## written by Ashton Shortridge, May/June, 2008.


# nscore ------------------------------------------------------------------
nscore <- function(x) {
  # Takes a vector of values x and calculates their normal scores. 
  nscore <- qqnorm(x, plot.it = FALSE)$x  # normal score 
  trn.table <- data.frame(x=sort(x),nscore=sort(nscore))
  return(nscore)
}
