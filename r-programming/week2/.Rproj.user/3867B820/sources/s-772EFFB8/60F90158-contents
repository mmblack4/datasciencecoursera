complete <- function(directory, id=1:332){
  allFile <- list.files(directory, full.names = TRUE)
  selectedData <- data.frame()
  completeCases <- data.frame()
  nobs <- data.frame()
  for(i in id){
    selectedData <- read.csv(allFile[i],header = TRUE)
    nobs <- sum(complete.cases(selectedData))
    completeCases<-rbind(completeCases,data.frame(i,nobs))
  }
  completeCases
}