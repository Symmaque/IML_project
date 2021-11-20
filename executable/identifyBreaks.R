# Two methods

# BP method : statistics

# Data since 1970 --> max 2 breaks
# Data since 1955 --> max 3 breaks
# Data before 1955 --> max 4 breaks

source("./lib/identifyBreaksLibrary.R")

breaks <- identifyBreaks()

breaksData <- getBreaksData()

