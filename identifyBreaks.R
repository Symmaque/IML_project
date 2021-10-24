# Two methods

# BP method : statistics

# Data since 1970 --> max 2 breaks
# Data since 1955 --> max 3 breaks
# Data before 1955 --> max 4 breaks

fetchData <- function(){
  data <- read.csv2("pwt71_wo_country_names_wo_g_vars.csv", sep = ",", dec = ".")
  data <- data[,c("isocode","year","rgdpl")]
  data <- na.omit(data)
}
getMaximumBreaks <- function (x, start){
  if(start >= 1970){
    return(2)
  }else if (start >= 1955){
    return(3)
  }else{
    return(4)
  }
}
getCountryData<- function (x, isocode){
  x[x$isocode == isocode,]
}
plotData <- function (x){
  plot(x$year, log(x$rgdpl), type = "l")
}
getFirstBreak <- function (x){
  #x <- ts(x$rgdpl, start = 1950, end = 2010)  #convert into time serie
  #time <- 1:61
  #breakpoints(log(x) ~ time, h = 7)
  breakpoints(log(x$rgdpl)~x$year)
}
getGrowth <- function(x, start, end){
  y <- NULL
  for (t in 1:(end-start-1)){
    y[t] <- (x$rgdpl[t+1] - x$rgdpl[t])/x$rgdpl[t]
  }
  return(y)
}
getGrowthEpisode <- function (growth, breakpoints, nbBreaks){
  if(nbBreaks == 0){
    return (mean(growth))
  }
  growthBetweenBreakdates <- NULL
  growthBetweenBreakdates[1] <- mean(growth[1:(breakpoints[1]-1)])
  if (nbBreaks >= 2){
    for (t in 2:nbBreaks){
    growthBetweenBreakdates[t] <- mean(growth[breakpoints[t-1]:(breakpoints[t]-1)])
    }
  }
  growthBetweenBreakdates[nbBreaks+1] <- mean(growth[breakpoints[nbBreaks]:length(growth)])
  return(growthBetweenBreakdates*100)
}
verifyBreaks <- function(growthBetweenBreakdates, breakdates, nbBreaks){
  realBreakdates <- NULL
  breakType <- NULL
  realBreakdatesType <- NULL
  if (nbBreaks == 0){
    return(realBreakdates)
  }

  breakType[1] <- sign(growthBetweenBreakdates[2]-growthBetweenBreakdates[1])
  if(abs(growthBetweenBreakdates[2]-growthBetweenBreakdates[1]) > 2){
    realBreakdates[1] <- breakdates[1]
    realBreakdatesType[1] <- breakType[1]
  }
  if(nbBreaks == 1){
    return(list(realBreakdates = realBreakdates, realBreakdatesType = realBreakdatesType))
  }
  for (t in 2:nbBreaks){
    transition <- growthBetweenBreakdates[t+1]-growthBetweenBreakdates[t]
    breakType[t] <- sign(transition)
    # if same type and change > 1 ppa
    if (breakType[t-1] == breakType[t] & abs(transition) > 1){
      realBreakdates <- c(realBreakdates, breakdates[t])  #confirm the break
      realBreakdatesType <- c(realBreakdatesType, breakType[t])
    }else if (breakType[t-1] != breakType[t] & abs(transition) > 3){
      realBreakdates <- c(realBreakdates, breakdates[t])  #confirm the break
      realBreakdatesType <- c(realBreakdatesType, breakType[t])
    }
  }

  return(list(realBreakdates = realBreakdates, realBreakdatesType = realBreakdatesType))
}
getGenuineBreaks <- function(x){
  #get max breakdates
  start <- min(x$year)
  end <- max(x$year)  #supposed to always be 2010
  nbMaxBreaks <- getMaximumBreaks(x, start)
  #get breakpoints
  if(length(x$rgdpl) < 16){
    return(list(length(x$rgdpl),"mesures"))
  }
  breakpoints <- breakpoints(x$rgdpl~x$year, h = 8, breaks = nbMaxBreaks)

  breakpoints <- breakpoints[[1]]
  if (is.na(breakpoints[1])){
    return(list(NULL, NULL))
  }
  nbBreaks <- length(breakpoints)


  # get breakdates
  breakdates <- breakpoints + start - 1

  #get growth for each year
  growth <- getGrowth(x, start, end)

  #get growth for each episode
  growthBetweenBreakdates <- getGrowthEpisode(growth,breakpoints,nbBreaks)

  #return(list(levels(factor(x$isocode)),growthBetweenBreakdates,breakdates))

  #apply criteria on breakdates to indentify the genuine ones
  return(verifyBreaks(growthBetweenBreakdates, breakdates, nbBreaks))
}


isocodes <- levels(factor(data$isocode))
breaks <- NULL

for (isocode in isocodes){
  countryData <- getCountryData(data, isocode)
  resultCountry <- list(isocode = isocode, breaks = getGenuineBreaks(countryData))
  breaks <- c(breaks,resultCountry)
}

source("plotResults.R")

#plotData(dataBra)
#x <- getGenuineBreaks(dataBra)
# y <- getFirstBreak(dataBra)
# print(y)
# dataCan <- getCountryData(data,"CAN")
# maxCan <- getMaximumBreaks(dataCan)
# plotData(dataCan)
# x<- getFirstBreak(dataCan)

# Comparing with GDP