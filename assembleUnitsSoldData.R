assembleUnitsSoldData <- function(directory) {
  library(plyr)
  source("~/Dropbox/Data Science Stuff/rfunctions/ingestCSVsFromDirectory.R")
  
  ###########################################################################################################################
  ##                                                                                                                       ##
  ##                                                      Units Sold                                                       ##
  ##                                                                                                                       ##
  ###########################################################################################################################
  
  unitsSold <- ingestCSVsFromDirectory(directory, 10)
  tmpNames <- names(unitsSold)
  
  ## Do some cleaning and polishing...
  unitsSold <- lapply(seq(unitsSold), function(i) {
    y <- data.frame(unitsSold[[i]])
    
    ## Take only what we want
    y <- y[1:(nrow(y)-1), 1:2]
    
    ## Change the column names
    names(y) <- c("timestamp", names(unitsSold)[i])
    
    ## Force correct classes for columns
    y[, names(unitsSold)[i]] <- as.numeric(gsub(",", "", y[, 2]))
    y$timestamp <- as.Date(y$timestamp, "%m/%d/%y")
    
    ## Change the column names
    names(y) <- c("timestamp", paste(names(unitsSold)[i], "Units Sold", sep = " "))
    
    return(y)
  })
  names(unitsSold) <- tmpNames
  
  unitsSold
}