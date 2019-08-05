# Function: comb_dup.R
# By: Sabine Loos
# Latest Update: 07.31.2019

# DESCRIPTION: function to combine columns in dataframe that have the same names but have values in opposite rows

# FUNCTIONS: 
## comb_dup - combines two columns into one column

# INPUTS:
## dataframe = dataframe with two columns with same names, values in opposite rows

# OUTPUTS: 
## dataframe = dataframe with one column, values in all rows

# comb_dup ------------------------------------------------------------------
comb_dup <- function(dataframe){
  # find indices of duplicated columns
  d1 <- which(duplicated(names(dataframe)))
  d2 <- which(duplicated(names(dataframe), fromLast = T))
  # sum across rows (assuming NAs where no value is present)
  for (i in 1:length(d1)) {
    dataframe[d2[i]]<- rowSums(dataframe[,c(d1[i],d2[i])], na.rm = T)
  }
  # remove duplicated column
  dataframe <- dataframe[,-d1]
  return(dataframe)
}