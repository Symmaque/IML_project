library(strucchange)

getMaximumBreaks <- function (start){
  if(start >= 1970){
    return(2)
  }else if (start >= 1955){
    return(3)
  }else{
    return(4)
  }
}

getGrowthEpisode <- function(country, start_year, end_year){
  initialGDP <- data[data$isocode == country & data$year == start_year,"rgdpl"]
  finalGDP <- data[data$isocode == country & data$year == end_year,"rgdpl"]
  result <- (finalGDP - initialGDP)/initialGDP
  return(result)
}

getGrowthCountry <- function (breakpoints, nbBreaks, country){
  data_country <- data[data$isocode == country,]
  first_year <- min(data_country$year)
  last_year <- max(data_country$year)

  if(nbBreaks == 0){
    #return (mean(growth, na.rm = T))
    return (getGrowthEpisode(country, first_year, last_year))
  }
  growthBetweenBreakdates <- NULL
  growthBetweenBreakdates[1] <- getGrowthEpisode(country, first_year, breakpoints[1]+first_year+1)
  #growthBetweenBreakdates[1] <- mean(growth[2:(breakpoints[1])])
  if (nbBreaks >= 2){
    for (t in 2:nbBreaks){
      #growthBetweenBreakdates[t] <- mean(growth[breakpoints[t-1]:(breakpoints[t]-1)])
      growthBetweenBreakdates[t] <- getGrowthEpisode(country, breakpoints[t-1]+first_year+1, breakpoints[t]+first_year+1)
    }
  }
  #growthBetweenBreakdates[nbBreaks+1] <- mean(growth[breakpoints[nbBreaks]:length(growth)])
  growthBetweenBreakdates[nbBreaks+1] <- getGrowthEpisode(country, breakpoints[nbBreaks]+first_year+1, last_year)
  return(growthBetweenBreakdates)
}

verifyBreaks <- function(growthBetweenBreakdates, breakdates, nbBreaks){
  realBreakdates <- NULL
  breakType <- NULL
  realBreakdatesType <- NULL
  if (nbBreaks == 0){
    return(realBreakdates)
  }

  breakType[1] <- sign(growthBetweenBreakdates[2]-growthBetweenBreakdates[1])
  #0.084
  if(abs(growthBetweenBreakdates[2]-growthBetweenBreakdates[1]) > 0.084){
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
    #0.042
    if (breakType[t-1] == breakType[t] & abs(transition) > 0.042){
      realBreakdates <- c(realBreakdates, breakdates[t])  #confirm the break
      realBreakdatesType <- c(realBreakdatesType, breakType[t])
    }else if (breakType[t-1] != breakType[t] & abs(transition) > 0.126){ #0.126
      realBreakdates <- c(realBreakdates, breakdates[t])  #confirm the break
      realBreakdatesType <- c(realBreakdatesType, breakType[t])
    }
  }

  return(list(realBreakdates = realBreakdates, realBreakdatesType = realBreakdatesType))
}

getGenuineBreaks <- function(country_data){
  #get max breakdates
  start <- min(country_data$year)
  end <- max(country_data$year)  #supposed to always be 2010
  nbMaxBreaks <- getMaximumBreaks(start)
  #get breakpoints
  breakpoints <- breakpoints(country_data$rgdpl~country_data$year, h = 8, breaks = nbMaxBreaks)

  breakpoints <- breakpoints[[1]]
  if (is.na(breakpoints[1])){
    return(list(NULL, NULL))
  }
  nbBreaks <- length(breakpoints)


  # get breakdates
  breakdates <- breakpoints + start - 1

  countryName <- country_data[[1]][1]
  #get growth for each episode
  growthBetweenBreakdates <- getGrowthCountry(breakpoints, nbBreaks, countryName)


  #return(list(levels(factor(x$isocode)),growthBetweenBreakdates,breakdates))

  #apply criteria on breakdates to indentify the genuine ones
  return(verifyBreaks(growthBetweenBreakdates, breakdates, nbBreaks))
}

#run methods for each country
identifyBreaks <- function (){
  countries <- levels(factor(data$isocode))
  nb_countries <- length(countries)
  #breaks <- list(isocode = NULL, breaks = NULL)
  breaks <- replicate(nb_countries, list(), F)
  names(breaks) <- countries
  for (country in countries){
    countryData <- data[data$isocode == country,]
    breaks[[country]] <- getGenuineBreaks(countryData)
  }
  return(breaks)
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
