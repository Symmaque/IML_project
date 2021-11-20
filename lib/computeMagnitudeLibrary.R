getEpisodeMagnitude <- function(g_ep, g_prm, N_ep){
  return((g_ep - g_prm)*N_ep)
}

getPRMGrowth_Episode <- function(start, end, data_country, countries, start_previous, end_previous, previous_growth){
  #data_country contains gdp and year for the country
  #countries contains the names of every country in the world
  #start and end of the episode
  first_year <- min(data_country$year)
  last_year <- max(data_country$year)
  country_name <- data_country[[1]][1]
  alpha <- getGrowthEpisode(country_name, first_year, last_year)
  gamma <- getGrowthEpisode(country_name, start, end)
  beta <- getWAGrowth(countries, start, end) #current wa growth
  initial_level_income <- log(data_country[data_country$year == start,"rgdpl"])  #initial level of income
  #We don't care about above or below because it's the same formula whatever

  previous_complete_regression <- 1
  if(is.null(previous_growth) == F){  #start_previous and end_previous are not null if previous_growth is not
    gwa_before <- getWAGrowth(countries, start_previous, end_previous) #previous wa growth
    previous_complete_regression <- (previous_growth - gwa_before)
  }
  growth_prm <- alpha + beta * previous_complete_regression + gamma * initial_level_income
  return(growth_prm)
}



#' alpha : growth for the entire time serie
#' beta : g_wa during the episode
#' gamma : growth during the episode

getPRMGrowth_Country <- function(country){

  subset <- breaksData[breaksData$countryName == country,]

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
  return(list(country = rep(country,nb_breaks+1), growth_prm = prm_growth_list, duration = intervals, growth_real = real_growth_list))
}

getCountryMagnitudeEpisode <- function(country){
  growth <- getPRMGrowth_Country(country)
  growth$magnitude <- getEpisodeMagnitude(growth$growth_real, growth$growth_prm, growth$duration)
  return(growth)
}

getMagnitudes <- function(countries){
  nb_countries <- length(countries)
  magnitudesList <- replicate(nb_countries, list())
  names(magnitudesList) <- countries
  for (country in countries){
    magnitudesList[[country]] <- getCountryMagnitudeEpisode(country)
  }
  return(magnitudesList)
}

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