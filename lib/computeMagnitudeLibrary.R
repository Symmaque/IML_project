#' computes the magnitude of episodes
#' @param g_ep real growth of the episodes
#' @param g_prm counter factual growth of the episodes
#' @param N_ep duration of the episodes in years
#' @return magnitude of episodes
getEpisodeMagnitude <- function(g_ep, g_prm, N_ep){
  return((g_ep - g_prm)*N_ep)
}

#' computes the counter factual growth for an episode of a country
#' @param start first year of the episode
#' @param end last year of the episode
#' @param data_country table containing data for the country to which episode belong
#' @param countries list of countries in the world
#' @param start_previous the first year of the previous episode (NULL if the episode is the first)
#' @param end_previous the last year of the previous episode (NULL if the episode is the first)
#' @param previous_growth the real growth of the previous episode (NULL if the episode is the first)
#' @return the counter factual growth for the episode
getPRMGrowth_Episode <- function(start, end, data_country, countries, start_previous, end_previous, previous_growth){

  #start and end of the episode
  first_year <- min(data_country$year)
  last_year <- max(data_country$year)

  country_name <- data_country[[1]][1]

  #alpha : growth for the entire time serie
  #beta : g_wa during the episode
  #gamma : growth during the episode
  alpha <- getGrowthEpisode(country_name, first_year, last_year)
  gamma <- getGrowthEpisode(country_name, start, end)
  beta <- getWAGrowth(countries, start, end) #current wa growth
  initial_level_income <- log(data_country[data_country$year == start,"rgdpl"])  #initial level of income

  previous_complete_regression <- 1

  if(is.null(previous_growth) == F){  #start_previous and end_previous are not null if previous_growth is not
    gwa_before <- getWAGrowth(countries, start_previous, end_previous) #previous wa growth
    previous_complete_regression <- (previous_growth - gwa_before)
  }

  growth_prm <- alpha + beta * previous_complete_regression + gamma * initial_level_income #formula for counter factual growth

  return(growth_prm)
}

#' computes the counter factual growth for every episode of a country
#' @param country the isocode of the country
#' @return the list of the real growths, counter factual growths and duration of the episodes of the country
getPRMGrowth_Country <- function(country){

  subset <- breaksData[breaksData$countryName == country,]  #taking data only for this country

  nb_breaks <- length(subset$breakdates)

  data_country <- data[data$isocode == country,]
  first_year_country <- min(data_country$year)
  last_year_country <- max(data_country$year)
  world_countries <- levels(factor(data$isocode))

  dates <- c(first_year_country,subset$breakdates,last_year_country)

  prm_growth_first <- getPRMGrowth_Episode(first_year_country,subset$breakdates[1],data_country,world_countries,NULL,NULL,NULL)
  real_growth_first <- getGrowthEpisode(country, first_year_country, subset$breakdates[1])

  prm_growth_list <- prm_growth_first
  real_growth_list <- real_growth_first

  for (i in 2:(nb_breaks+1)){
    prm_growth <- getPRMGrowth_Episode(dates[i],dates[i+1],data_country,world_countries,dates[i-1],dates[i],prm_growth_list[i-1])
    prm_growth_list <- c(prm_growth_list, prm_growth)
    real_growth <- getGrowthEpisode(country, dates[i], dates[i+1])
    real_growth_list <- c(real_growth_list, real_growth)
  }

  intervals <- as.numeric(dates[2:length(dates)]) - as.numeric(dates[1:(length(dates)-1)])
  return(list(
    country = rep(country,nb_breaks+1),
    growth_prm = prm_growth_list,
    duration = intervals,
    growth_real = real_growth_list))
}

#' computes the magnitude of all episodes for a country
#' @param country the isocode of the country
#' @return the list of real growths, counter factual growths, durations and magnitudes of the country's episodes
getCountryMagnitudeEpisode <- function(country){
  growth <- getPRMGrowth_Country(country)
  growth$magnitude <- getEpisodeMagnitude(growth$growth_real, growth$growth_prm, growth$duration)
  return(growth)
}

#' computes the magnitudes of the episodes of every countries
#' @param countries the list of countries
#' @return the list of real growths, counter factual growths, durations and magnitudes of the countries' episodes
getMagnitudes <- function(countries){
  nb_countries <- length(countries)
  magnitudesList <- replicate(nb_countries, list())
  names(magnitudesList) <- countries
  for (country in countries){
    magnitudesList[[country]] <- getCountryMagnitudeEpisode(country)
  }
  return(magnitudesList)
}

#' computes the world average growth during an episode
#' @param countries the list of countries in the world
#' @param year_start the first year of the episode
#' @param year_end the last year of the episode
#' @return the average growth of all the countries during the episode
getWAGrowth <- function(countries, year_start, year_end){
  initial <- final <- NULL
  for (country in countries){
    country_first_year <- min(data[data$isocode == country, "year"])
    country_last_year <- max(data[data$isocode == country, "year"])

    if(country_first_year > year_start | country_last_year < year_end){ #excluding countries with partial data for this period
      next
    }

    countryGDP_initial <- data[data$isocode == country & data$year == year_start, "rgdpl"]
    countryGDP_final <- data[data$isocode == country & data$year == year_end, "rgdpl"]

    initial <- c(initial, countryGDP_initial)
    final <- c(final, countryGDP_final)
  }
  return ((mean(final)-mean(initial))/mean(initial))
}