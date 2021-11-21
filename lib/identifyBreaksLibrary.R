library(strucchange)  #needed to apply Bai Perron tests

#' computes the maximum of breaks
#' @param start the first year of data
#' @return the maximum number of breaks
getMaximumBreaks <- function (start){
  if(start >= 1970){
    return(2)
  }else if (start >= 1955){
    return(3)
  }else{
    return(4)
  }
}

#' computes the growth for an episode
#' @param country isocode of the country
#' @param start_year first year of the episode
#' @param end_year last year of the episode
#' @return average growth per year during the episode
getGrowthEpisode <- function(country, start_year, end_year){
  currentGDP <- data[data$isocode == country & data$year > start_year & data$year <= end_year, "rgdpl"]
  previousGDP <- data[data$isocode == country & data$year >= start_year & data$year < end_year, "rgdpl"]
  growth <- (currentGDP - previousGDP)/previousGDP
  return(mean(growth))
}

#' computes the growth for all episodes for a country
#' @param breakdates list of breakdates for the country
#' @param nbBreaks the number of breaks
#' @param country the isocode of the country
#' @return list of growths between the breakdates
getGrowthCountry <- function (breakdates, nbBreaks, country){
  data_country <- data[data$isocode == country,]
  first_year <- min(data_country$year)
  last_year <- max(data_country$year)

  breakdates <- c(first_year, breakdates, last_year)

  #initiate list
  growthBetweenBreakdates <- NULL

  #episodes
    for (t in 1:(nbBreaks+1)){
      growthBetweenBreakdates[t] <- getGrowthEpisode(country, breakdates[t], breakdates[t+1])
    }

  return(growthBetweenBreakdates)
}

#' filters the breakdates depending on the certain criteria
#' @param growthBetweenBreakdates the list of growths between breakdates
#' @param breakdates the list of breakdates
#' @param nbBreaks the number of breaks
#' @return list of real breakdates and their type (acceleration or decelaration)
verifyBreaks <- function(growthBetweenBreakdates, breakdates, nbBreaks){
  #initiate lists
  realBreakdates <- NULL
  breakType <- NULL
  realBreakdatesType <- NULL

  if (nbBreaks == 0){ #if no break, returns NULL
    return(realBreakdates)
  }

  #first break
  breakType[1] <- sign(growthBetweenBreakdates[2]-growthBetweenBreakdates[1])
  if(abs(growthBetweenBreakdates[2]-growthBetweenBreakdates[1]) > 0.02){
    realBreakdates[1] <- breakdates[1]
    realBreakdatesType[1] <- breakType[1]
  }
  if(nbBreaks == 1){  #if only one break
    return(list(realBreakdates = realBreakdates, realBreakdatesType = realBreakdatesType))
  }

  #intermediate breaks
  for (t in 2:nbBreaks){
    transition <- growthBetweenBreakdates[t+1]-growthBetweenBreakdates[t]
    breakType[t] <- sign(transition)

    #two consecutive breaks of the same type
    if (breakType[t-1] == breakType[t] & abs(transition) > 0.01){
      realBreakdates <- c(realBreakdates, breakdates[t])  #confirm the break
      realBreakdatesType <- c(realBreakdatesType, breakType[t])
    }else if (breakType[t-1] != breakType[t] & abs(transition) > 0.03){ #two consecutive breaks of different types
      realBreakdates <- c(realBreakdates, breakdates[t])  #confirm the break
      realBreakdatesType <- c(realBreakdatesType, breakType[t])
    }
  }

  return(list(
    realBreakdates = realBreakdates,
    realBreakdatesType = realBreakdatesType))
}

#' computes the real breakdates for the country whose data is country_data
#' @param country_data table containing the data for the country
getGenuineBreaks <- function(country_data){
  start <- min(country_data$year) #first year of data

  nbMaxBreaks <- getMaximumBreaks(start)  #max number of breaks
  #get breakpoints
  breakpoints <- breakpoints(country_data$rgdpl~country_data$year, h = 8, breaks = nbMaxBreaks)
  #selecting only the breakpoints
  breakpoints <- breakpoints[[1]]

  #if there is no break
  if (is.na(breakpoints[1])){
    return(list(NULL, NULL))
  }

  nbBreaks <- length(breakpoints)

  # get breakdates from breakpoints
  breakdates <- breakpoints + start - 1

  countryName <- country_data[[1]][1]
  #get growth for each episode
  growthBetweenBreakdates <- getGrowthCountry(breakdates, nbBreaks, countryName)

  #apply criteria on breakdates to indentify the genuine ones
  return(verifyBreaks(growthBetweenBreakdates, breakdates, nbBreaks))
}

#' indentifies breaks for all coutries
#' @return list of breaks
identifyBreaks <- function (){
  countries <- levels(factor(data$isocode))
  nb_countries <- length(countries)

  #initates the list
  breaks <- replicate(nb_countries, list(), F)
  names(breaks) <- countries

  for (country in countries){
    countryData <- data[data$isocode == country,]
    breaks[[country]] <- getGenuineBreaks(countryData)
  }

  return(breaks)
}

#' creates a dataframe from the list of breaks and writes it in a csv file
#' @return dataframe of breaks
getBreaksData <- function (){
  #fetch all breakdates
  breakdates <- unname(unlist(sapply(breaks, "[[", 1))) #get a vector with alla breakdates
  #fetch all breakTypes
  breakType <- unname(unlist(sapply(breaks, "[[", 2)))  #get a vector with all break types
  #fetch the list of nuber of breaks
  nbBreaks <- lengths(sapply(breaks, "[[", 1))          #get a vector with the nb of breaks for each country
  #fecth the list of countries
  countries <- levels(factor(data$isocode))

  countryNames <- rep(countries[nbBreaks != 0],nbBreaks[nbBreaks != 0]) #rep each country its number of breaks times

  #build the matrix of countries, breakdates and breakTypes
  resMatrix <- cbind(countryNames, breakdates, breakType)
  colnames(resMatrix) <- c("countryName", "breakdates", "breakType")

  #get the data frame
  breaksData <- as.data.frame(resMatrix)
  breaksData$breakdates <- as.numeric(breaksData$breakdates) #cast in numbers
  breaksData$breakType <- as.numeric(breaksData$breakType)  #cast in numbers

  #write the matrix in the csv file
  write.table(resMatrix, "./results/Country breaks.csv")

  return(breaksData)
}
