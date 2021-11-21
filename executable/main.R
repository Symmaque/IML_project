# Title     : IML_project
# Objective : Identify and compute magnitude of growth episodes
# Created by: Aurélien
# Created on: 13/10/2021

#' fetch and clean raw data
fetchData <- function(){
  data <- read.csv2("./data/pwt71_wo_country_names_wo_g_vars.csv", sep = ",", dec = ".")
  data <- data[,c("isocode","year","rgdpl","POP")] #keeps only 4 columns
  data <- na.omit(data) #omit NA values

  #lists of countries to exclude
  young_countries <- NULL
  small_countries <- NULL

  for (iso in levels(factor(data$isocode))){
    if (min(data[data$isocode == iso,"year"]) > 1970){ #Data since 1970
      young_countries <- c(young_countries, iso)
      next
    }
    if(data[data$isocode == iso & data$year == 1980,"POP"] < 700){  #Population less than 700k in 1980
      small_countries <- c(small_countries, iso)
      next
    }
  }
  #excluding countries
  deleted_countries <- c(young_countries, small_countries)
  data <- data[(data$isocode %in% deleted_countries) == FALSE,]
}

#cleaned data frame
data <- fetchData()

#identify breaks/episodes
source("./executable/identifyBreaks.R")

#compute magnitude for each episode
source("./executable/computeMagnitude.R")