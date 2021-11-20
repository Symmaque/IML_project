#load functions
source("./lib/identifyBreaksLibrary.R")

# list  of breaks for every country
breaks <- identifyBreaks()

# data frame of breaks for every country and writing in csv file
breaksData <- getBreaksData()

