best <- function(state,outcome){
  #Loading data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  #check state is valid
  if(state %in% sapply(data["State"],unique)){
    #select the state
    temp<-data[data$State==state,]
    
    if(identical(outcome,"heart attack")){
      #convert Death rate to numeric value
      temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]<-as.numeric(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])

      #order a datafram base on Rate
      index<-order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)[1]
    }
    
    else if(identical(outcome,"heart failure")){
      #convert Death rate to numeric value
      temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]<-as.numeric(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"])

      #order a datafram base on Rate
      index <- order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)[1]
    }

    else if(identical(outcome,"pneumonia")){
      #convert Death rate to numeric value
      temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"]<-as.numeric(temp[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"])

      #order a datafram base on Rate
      index <- order(temp$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)[1]
    }

    else{
      stop('invalid outcome')
    }

  }

  else{
    stop('invalide state')
  }

  #return hospital name
  temp[index,2]
}