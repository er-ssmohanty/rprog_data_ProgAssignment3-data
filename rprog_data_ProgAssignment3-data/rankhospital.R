rankhospital <- function(state, outcome, num = "best") {
  #data collection and selection
  outpd<-read.csv("outcome-of-care-measures.csv")##file location--editable
  target_columns=names(outpd)[c(2,11,17,23)]
  source("../na-cleaner.R")
  
  if(!is.element(state,outpd$State)){
    return('invalid state')
  }
  
  #data cleaning
  target_data=subset(outpd,State==state,select = target_columns)
  target_data[,2] <- as.numeric(as.character(target_data[,2]))
  target_data[,3] <- as.numeric(as.character(target_data[,3]))
  target_data[,4] <- as.numeric(as.character(target_data[,4]))
  #target_data=cleana(target_data)
  
  
  if(outcome=='heart attack'){
    slno=2
  }
  else if(outcome=='heart failure'){
    slno=3
  }
  else if(outcome=='pneumonia'){
    slno=4
  }
  else{
    return('invalid outcome')
  }

  target_data=subset(target_data,select=names(target_data)[c(1,slno)])
  target_data=cleana(target_data)
  target_data=target_data[order(target_data[,2],target_data[,1]),]

  if(num=='best'){
    num=1
  }
  else if(num=='worst'){
    num=dim(target_data)[1]
  }
  else{
    num=num
  }
  #target_value=target_data[1,1]

  return(target_data[num,1])
  #return(target_data[1])
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}
