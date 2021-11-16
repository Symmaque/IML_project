source("./lib/computeMagnitudeLibrary.R")

#Getting the growth during the episode (done in identifyBreaks)
#Compare the actual growth with the counter factual growth
#3 counter factual growth methods:
  #No regression to mean : same growth as the previous episode
  #Complete regression to mean : same growth as the world average growth during this episode
  #Partial regression to mean (PRM)
  #