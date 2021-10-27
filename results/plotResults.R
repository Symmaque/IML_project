tab <- read.csv2("../data/isocodes.csv", sep = ",", quote = "\"")
results <- NULL
iso <- NULL
breakdates <- NULL
breakType <- NULL
countryNames <- NULL
nrows <- 0

getCountryName <- function(isocode){
  result <- tab[tab$Alpha.3.code == isocode,1]
  if(length(result) == 0){
    return(isocode)
  }
  return(result)
}

for (i in 0:189){
  currentBreaks <- breaks[[(2*i + 2)]]$realBreakdates
  breakdates <- c(breakdates,currentBreaks)
  breakType <- c(breakType,breaks[[(2*i + 2)]]$realBreakdatesType)
  nbBreaks <- length(currentBreaks)
  #countryName <- getCountryName(breaks[[(2*i + 1)]])
  countryName <- breaks[[(2*i + 1)]]
  countryNames <- c(countryNames,rep(countryName,nbBreaks))
  nrows <- nrows + nbBreaks
}

#TODO : write the matrix in a csv file
resMatrix <- cbind(countryNames, breakdates, breakType)
write.table(resMatrix, "Country breaks.csv")