# Title     : IML_project
# Objective : Implement the data processing described in the papers
# Created by: Aurélien
# Created on: 13/10/2021

# Identify episodes equivalent to indentifing breaks
# calculate metrics for each episode

# Need to get the Ln(GDPPC) by year for every country

# GDPPC = GDP/population
# GDP = Consumption + Investment + Government Spending + Net Exports
# Net exports = Exports - Imports
# GDDPC = rgdpl
fetchData <- function(){
  data <- read.csv2("./data/pwt71_wo_country_names_wo_g_vars.csv", sep = ",", dec = ".")
  data <- data[,c("isocode","year","rgdpl","POP")]
  #data <- data[,c("isocode","year","rgdpl","rgdpl2","rgdpch","POP")]
  data <- na.omit(data)
  young_countries <- NULL
  small_countries <- NULL
  for (iso in levels(factor(data$isocode))){
    if (min(data[data$isocode == iso,2]) > 1970){ #Data since 1970
      young_countries <- c(young_countries, iso)
      next
    }
    if(data[data$isocode == iso & data$year == 1980,4] < 700){  #Population less than 700k in 1980
      small_countries <- c(small_countries, iso)
      next
    }
  }
  deleted_countries <- c(young_countries, small_countries)
  data <- data[(data$isocode %in% deleted_countries) == FALSE,]
}

data <- fetchData()

#identify breaks/episodes
source("./executable/identifyBreaks.R")

#compute magnitude for each episode
source("./executable/computeMagnitude.R")


