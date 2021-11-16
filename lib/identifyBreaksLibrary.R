library(strucchange)
getMaximumBreaks <- function (x, start){
  if(start >= 1970){
    return(2)
  }else if (start >= 1955){
    return(3)
  }else{
    return(4)
  }
}

getCountryData<- function (country){
  data[data$isocode == country,]
}

getGrowthCountry <- function(data_country, start_index, end_index){
  growth <- NULL
  growth[1] <- NULL
  for (t in 2:(end_index-start_index)){
    growth[t] <- (data_country$rgdpl[t] - data_country$rgdpl[t-1])/data_country$rgdpl[t-1]
  }
  return(growth)
}

# TODO : see if this function works and is usefull
getGrowthCountry2 <- function(country, start_index, end_index){
  return(growthData[[country]]$growth[start_index:end_index])
}


getGrowthEpisode <- function (growth, breakpoints, nbBreaks){
  if(nbBreaks == 0){
    return (mean(growth, na.rm = T))
  }
  growthBetweenBreakdates <- NULL
  growthBetweenBreakdates[1] <- mean(growth[2:(breakpoints[1])])
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
getGenuineBreaks <- function(country_data){
  #get max breakdates
  start <- min(country_data$year)
  end <- max(country_data$year)  #supposed to always be 2010
  nbMaxBreaks <- getMaximumBreaks(country_data, start)
  #get breakpoints




  breakpoints <- breakpoints(country_data$rgdpl~country_data$year, h = 8, breaks = nbMaxBreaks)

  breakpoints <- breakpoints[[1]]
  if (is.na(breakpoints[1])){
    return(list(NULL, NULL))
  }
  nbBreaks <- length(breakpoints)


  # get breakdates
  breakdates <- breakpoints + start - 1

  #get growth for each year
  growth <- getGrowthCountry(country_data, start, end)

  #get growth for each episode
  growthBetweenBreakdates <- getGrowthEpisode(growth,breakpoints,nbBreaks)

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
    countryData <- getCountryData(country)
    breaks[[country]] <- getGenuineBreaks(countryData)
  }
  return(breaks)
}

getGrowthList <- function(){
  countries <- levels(factor(data$isocode))
  nb_countries <- length(countries)
  result <- replicate(nb_countries, list(),F)
  names(result) <- countries
  for (country in countries){
    country_data <- getCountryData(country)
    start <- min(country_data$year)
    end <- max(country_data$year)
    growth <- getGrowthCountry(country_data, start, end)
    result[[country]]$growth <- growth
    result[[country]]$years <- country_data$year
  }
  return(result)
}