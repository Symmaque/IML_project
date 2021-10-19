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

data <- read.csv2("pwt71_wo_country_names_wo_g_vars.csv", sep = ",", dec = ".")
factor(data$isocode)
factor(data$year)
na.omit(data)

lnGDPPC <- log(data$cgdp)
x <- subset.data.frame(data, isocode == "CAN")
y <- subset.data.frame(data, isocode == "JOR")

xx <- data[data$isocode == "CAN",]
plot(xx$year, xx$ppp,type = "l")
plot(xx$year, log(xx$rgdpl),type = "l")
plot(y$year, log(y$rgdpl),type = "l")
print(max(log(x$cgdp/x$POP)))
isocode <- data$isocode
