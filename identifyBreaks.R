# Two methods

# BP method : statistics

# Data since 1970 --> max 2 breaks
# Data since 1955 --> max 3 breaks
# Data before 1955 --> max 4 breaks
library(graphics)
dataTest <- data[data$isocode == "CAN",]
a <- 2
x <- list(c(2,4,6))
plot()

getMaximumBreaks <- function (x){
  stopifnot(typeof(x) == "list")
  start <- min(x$year)
  if(start >= 1970){
    2
  }else if (start >= 1955){
    3
  }else{
    4
  }
}


getCountryData<- function (x, isocode){
  x[x$isocode == isocode,]
}

plotData <- function (x){
  plot(x$year, log(x$rgdpl), type = "l")
}

getFirstBreak <- function (x){
  yearMin <- min(x$year)
  yearMax <- max(x$year)
  growth_before <- (log(x$rgdpl) - log(x$rgdpl[x$year == yearMin]))/log((x$rgdpl[x$year == yearMin]))
  growth_after <- (log(x$rgdpl[x$year == yearMax]) - log(x$rgdpl))/log(x$rgdpl)
  growth <- list(growth_before,growth_after)
  ecart <- abs(growth[[1]] - growth[[2]]) > 0.02
}

a <- x$isocode
dataCan <- getCountryData(data,"CAN")
maxCan <- getMaximumBreaks(dataCan)
plotData(dataCan)
x<- getFirstBreak(dataCan)
# Comparing with GDP