rankall <- function(outcome, num = "best") {
  #data collection and selection
  outpd<-read.csv("outcome-of-care-measures.csv")##file location--editable
  #target_columns=names(outpd)[c(2,)]
  source("~/R/source/na-cleaner.R")
  
  #
  if(outcome=='heart attack'){
    slno=11
  }
  else if(outcome=='heart failure'){
    slno=17
  }
  else if(outcome=='pneumonia'){
    slno=23
  }
  else{
    return('invalid outcome')
  }
  state_list=sort(unique(as.character(outpd[,7])))
  target_columns=names(outpd)[c(2,7,slno)]
  outpd=subset(outpd,select = target_columns)
  outpd[,3] <- as.numeric(as.character(outpd[,3]))
  outpd[,2] <- as.character(outpd[,2])
  outpd[,1] <- as.character(outpd[,1])
  outpd=cleana(outpd)
  target_data=outpd
  
  #
  
  #data cleaning
  answer=c()
  for(i in state_list){
    target_data=subset(outpd,State==i)
    if(num=='best'){
      num=1
    }
    else if(num=='worst'){
      num=dim(target_data)[1]
    }
    else{
      num=num
    }
    target_data=target_data[order(target_data[,3],target_data[,1]),]
    #print(target_data[num,1])
    answer=rbind(answer,c(target_data[num,1],i))
    #print(dim(answer))
  }
  #print(target_data[20,1])
  #return(0)
  #print(length(hospital_list),length(state_list))
  answer=data.frame(answer)
  names(answer)=c("hospital", "state")
  return(answer)
  return(target_data)
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}