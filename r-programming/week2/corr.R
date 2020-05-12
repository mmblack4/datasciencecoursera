corr <- function(directory, threshold = 0) {
  source("complete.R")
  completeCase <- complete(directory)
  cassAboveThreshold <- completeCase[completeCase$nobs > threshold,1]
  allFiles <- list.files(path=directory,full.names = TRUE)
  correlations <- rep(NA,length(cassAboveThreshold))
  for(i  in cassAboveThreshold){
    fileData <- read.csv(allFiles[i])
    completeCases <- complete.cases(fileData)
    validSulfateData <- fileData[completeCases,2]
    validNitrateData <- fileData[completeCases,3]
    correlations[i] <- cor(x=validSulfateData,y=validNitrateData)
  }
  correlations <-correlations[complete.cases(correlations)]
}