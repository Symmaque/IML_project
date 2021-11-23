#file to plot result from the list of countries

#list of countries
countries <- c("AFG", "BRA", "ITA", "JOR", "ROM")

#' adds vertical lines for the breakdates
#' @param country the isocode of the country
#' @param colors a vector of colors
addBreakLines <- function(country, colors){
  abline(v = breaksData[breaksData$country == country,"breakdates"], col = colors)
}

#' adds up or down arrows if the episode in an acceleration or a deceleration
#' @param country the isocode of the country
#' @param colors a vector of colors
#' @param middle.y the y coordinate of the middle of the plot
#' @param step.y a "short" distance on the y axis : height of the arrows
addArrows <- function(country, colors, middle.y, step.y){
  breakdates <- breaksData[breaksData$country == country,"breakdates"]

  start_arrows.y <- rep(middle.y, length(breakdates)) #every arrow starts from the middle
  start_arrows.x <- breakdates

  end_arrows.y <- start_arrows.y + breaksData[breaksData$country == country,"breakType"]*step.y #up if up break or down if down break
  end_arrows.x <- start_arrows.x #vertical arrows

  arrows(start_arrows.x, start_arrows.y, end_arrows.x, end_arrows.y, col = colors)
}

#' adds information about each episode
#' @param country the isocode of the country
#' @param min.x the min value on the x axis
#' @param max.x the max value on the x axis
addInformation <- function(country, min.x, max.x){
  #add min.x and max.x to breakdates to consider all intervals/episodes
  breakdates <- c(min.x,breaksData[breaksData$country == country,"breakdates"],max.x)

  length.x <- max.x - min.x #length of x axis

  #points in the middle of each episode
  middle_points <- (breakdates[2:length(breakdates)] + breakdates[1:(length(breakdates)-1)])/2
  #positions of the information in the middle of each episode
  positions <- (middle_points-min.x)/length.x

  #texts keeping 2 decimals
  growth_prm_text <- paste("g_PRM:",format(round(magnitudes[[country]]$growth_prm, 2), nsmall = 2))
  growth_real_text <- paste("g_real:",format(round(magnitudes[[country]]$growth_real,2), nsmall = 2))
  episode_magnitude_text <- paste("EM_ep:", format(round(magnitudes[[country]]$magnitude, 2), nsmall = 2))
  duration_text <- paste("N_ep:",magnitudes[[country]]$duration)

  #concatenated texts
  text <- paste0(growth_real_text, rep('\n', length(breakdates)), growth_prm_text, rep('\n', length(breakdates)), duration_text, rep('\n', length(breakdates)), episode_magnitude_text)

  mtext(text, adj = positions)
}

#' adds the values of breakdates on the x axis
#' @param country the isocode of the country
#' @param min.x the min value on the x axis
#' @param max.x the max value on the x axis
addBreakdates <- function(country, min.x, max.x){
  breakdates <- breaksData[breaksData$country == country & breaksData$breakdates %% 10 != 0,"breakdates"]
  length.x <- max.x - min.x #length of the x axis
  positions <- (breakdates-min.x)/length.x #positions of each breakdate
  mtext(paste0('\n','\n',breakdates), side = 1, adj = positions)
}

#' plot graphs for each country in the countries'list
plotResults <- function() {
  for(country in countries){
    X11() #new graph

    xlab <- "years"
    ylab <- "ln of GDP"

    #red if up break and blue if down break
    colors <- rep("red",length(breaksData[breaksData$country == country,"breakType"]))
    colors[breaksData[breaksData$country == country,"breakType"] == -1] <- "blue"

    max.y <- max(log(data[data$isocode == country,"rgdpl"]))
    min.y <- min(log(data[data$isocode == country,"rgdpl"]))
    middle.y <- (max.y + min.y)/2
    step.y <- (max.y - min.y)/5 #subjective value which is the height of arrows

    min.x <- min(data[data$isocode == country,"year"])
    max.x <- max(data[data$isocode == country,"year"])

    #plot data
    plot(log(data[data$isocode == country,"rgdpl"])~data[data$isocode == country,"year"], type = "l", xlab = xlab, ylab = ylab)

    #customize the graph
    addBreakLines(country, colors)
    addArrows(country, colors, middle.y, step.y)
    addInformation(country, min.x, max.x)
    addBreakdates(country, min.x, max.x)
  }
}

#plot results
plotResults()


