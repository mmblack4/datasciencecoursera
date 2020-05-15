rankhospital <- function(state,outcome,num="best"){
  #Loading data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  #check state is valid
  if(state %in% sapply(data["State"],unique)){
    #select the state
    temp<-data[data$State==state,]
    
    if(identical(outcome,"heart attack")){
      #convert Death rate to numeric value
      temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]<-as.numeric(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
      
      temp <- temp[,c(2,11)]
      temp <- temp[complete.cases(temp),]
      temp <- temp[order(temp$Hospital.Name,na.last=FALSE),]

      temp <- temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.last = FALSE),]
    }
    
    else if(identical(outcome,"heart failure")){
      #convert Death rate to numeric value
      temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]<-as.numeric(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])
      
      temp <- temp[,c(2,17)]
      temp <- temp[complete.cases(temp),]
      temp <- temp[order(temp$Hospital.Name,na.last=FALSE),]
      temp <- temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.last=FALSE),]
    }
    
    else if(identical(outcome,"pneumonia")){
      #convert Death rate to numeric value
      temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]<-as.numeric(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])
      
      temp <- temp[,c(2,23)]
      temp <- temp[complete.cases(temp),]
      temp <- temp[order(temp$Hospital.Name,na.last=FALSE),]
      
      temp <- temp[order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.last = FALSE),]
    }

    else{
      stop('invalid outcome')
    }

  }

  else{
    stop('invalide state')
  }
  
  #return hospital name
  if(identical(num,"best")){
      temp[num, 1]
  }
  else if(identical(num,"worst")){
      temp[nrow(temp),1]
  }
  else{
    temp[num,1]
  }
}