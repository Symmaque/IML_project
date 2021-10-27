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
  data <- read.csv2("../data/pwt71_wo_country_names_wo_g_vars.csv", sep = ",", dec = ".")
  data <- data[,c("isocode","year","rgdpl","rgdpl2","rgdpch")]
  data <- na.omit(data)
}

data <- fetchData()

#identify breaks/episodes
source("identifyBreaks.R")

#compute magnitude for each episode
source("computeMagnitude.R")


