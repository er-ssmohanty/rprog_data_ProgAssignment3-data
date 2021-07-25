best <- function(state, outcome) {
  #data collection and selection
  outpd<-read.csv("outcome-of-care-measures.csv")##file location--editable
  source("../na-cleaner.R")
  target_columns=names(outpd)[c(2,7,11,17,23)]
  target_data=subset(outpd,select = target_columns)
  #target_data=cleana(target_data)
  #print(target_columns)
  
  if(!is.element(state,unique(outpd$State))){
    return('invalid state')
  }
  
  #data cleaning
  target_data=subset(target_data,State==state)
  target_data[,1] <- as.character(target_data[,1])
  target_data[,2] <- as.character(target_data[,2])
  target_data[,3] <- as.numeric(as.character(target_data[,3]))
  target_data[,4] <- as.numeric(as.character(target_data[,4]))
  target_data[,5] <- as.numeric(as.character(target_data[,5]))
  #return(target_data)
  
  if(outcome=='heart attack'){
    slno=3
  }
  else if(outcome=='heart failure'){
    slno=4
  }
  else if(outcome=='pneumonia'){
    slno=5
  }
  else{
    return('invalid outcome')
  }
  target_columns=names(target_data)[c(1,slno)]
  #print(target_columns)
  target_data=subset(target_data,select = target_columns)
  target_data=cleana(target_data)
  #return(target_data)
  
  #sorting and finally solving
  target_data=target_data[order(target_data[,2]),]
  return(target_data[1,1])
  #IGNORE WHATEVER IT PRINTS AS OUTPUT AFTER THE FIRST LINE
  #THE FIRST LINE OF THE OUTPUT IS THE ANSWER
  #IF YOU CAN REMOVE THE UNWANTED LINES THEN SEND ME A PULL REQUEST
}
