plotData <- function (data){
  plot(data$year, log(data$rgdpl), type = "l")
}

getBreaksData <- function (){
  breakdates <- unname(unlist(sapply(breaks, "[[", 1))) #get a vector with alla breakdates
  breakType <- unname(unlist(sapply(breaks, "[[", 2)))  #get a vector with all break types
  nbBreaks <- lengths(sapply(breaks, "[[", 1))          #get a vector with the nb of breaks for each country
  countries <- levels(factor(data$isocode))                  #get the list of countries
  countryNames <- rep(countries[nbBreaks != 0],nbBreaks[nbBreaks != 0]) #rep each country it's number of breaks times

  resMatrix <- cbind(countryNames, breakdates, breakType)
  colnames(resMatrix) <- c("countryName", "breakdates", "breakType")
  breaksData <- as.data.frame(resMatrix)
  breaksData$breakdates <- as.numeric(breaksData$breakdates)
  breaksData$breakType <- as.numeric(breaksData$breakType)
  write.table(resMatrix, "./results/Country breaks.csv")
  return(breaksData)
}



