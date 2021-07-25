cleana <- function(dataframex){
  no_of_column = ncol(dataframex)
  target = !logical(nrow(dataframex)) #initialising a true-filled logical vector
  for(i in seq(no_of_column)){
    c = !is.na(aqd[,i])
    target = target&c   #if NA exists, changes target vector
  }
  cleaned_df = dataframex[target,]
  return(cleaned_df)
}