#' Useless to compute nb_countries*n²/2 periods
#' Should compute k*nb_countries with k is the avg of nb of breaks for each country
#' start : row index
#' end : column index

getCountryGrowthYear <- function(country, year){
  current_GDP <- data[data$isocode == country & data$year == year,"rgdpl"]
  previous_GDP <- data[data$isocode == country & data$year == (year-1),"rgdpl"]
  if (identical(numeric(0), current_GDP) | identical(numeric(0),previous_GDP)){ #at least one data missing for this year
    return(NULL)
  }
  growth <- (current_GDP - previous_GDP)/previous_GDP #compute growth
  return(growth)
}


getWAGrowth <- function(countries, start, end){
  period_growth <- NULL
  for (year in (start:end)){
    growth <- NULL
    for (country in countries){
      g_country <- getCountryGrowthYear(country,year)
      growth<-c(growth,g_country)
    }
    if (is.null(growth) == F){
      period_growth <- c(period_growth, mean(growth)) #add the mean of the growth of all the countries for this year
    }
  }
  if (is.null(period_growth) == F){
    mean(period_growth) #mean of the growth on years
  }else{
    NULL  #means that for the year asked, no country has data
  }
}

# wa_growth <- fillWAMatrix()