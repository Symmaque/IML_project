#load functions
source("./lib/computeMagnitudeLibrary.R")

# list of real growths, counter factual growths, duration and magnitudes for all countries' episodes
magnitudes <- getMagnitudes(levels(factor(breaksData$countryName)))