assembleSalesRankData <- function(directory, method = c("weekly", "daily", "hourly")) {
  library(plyr)
  source("~/Dropbox/Data Science Stuff/rfunctions/ingestCSVsFromDirectory.R")
  
  ###########################################################################################################################
  ##                                                                                                                       ##
  ##                                                      Sales Ranks                                                      ##
  ##                                                                                                                       ##
  ###########################################################################################################################
  
  ## import the raw amazon salesrank csvs into a list
  salesRanks <- ingestCSVsFromDirectory(directory)
  
  ## get their names, for use later when naming each list
  tmpNames <- names(salesRanks)
  
  ## Do some cleaning and polishing...
  salesRanks <- lapply(seq(salesRanks), function(i) {
    
    ## Get the dataframe in question
    y <- data.frame(salesRanks[[i]])
    
    ## Remove the last two rows and only take the first two columns,
    ## the timestamp and the sales rank from Amazon.com
    y <- y[1:(nrow(y)-2), 1:2]
    
    ## Change the column names to "timestamp" and the title of the book
    names(y) <- c("timestamp", names(salesRanks)[i])
    
    ## Force correct classes for columns. Basially make sure that the sales rank
    ## is a numeric. This introduces NAs by coersion, which is okay.
    y[, names(salesRanks)[i]] <- as.numeric(gsub(",", "", y[, 2]))
    
    ## Fix timestamp format
    if (method %in% c("daily", "weekly")) {
      y$timestamp <- as.Date(y$timestamp)  
    } else if (method == "hourly") {
      y$timestamp <- as.POSIXct(y$timestamp, tz = "")  
    }
    
    ## Select the lowest rank of each day/week
    if (method == "daily") {
      names(y)[2] <- "salesrank"
      y <- ddply(y, "timestamp", summarise, min(salesrank, na.rm = TRUE))
      names(y)[2] <- c(paste(names(salesRanks)[i], "Sales Rank", sep = " "))
    } else if (method == "weekly") {
      names(y)[2] <- "salesrank"
      y$week <- as.Date(cut(y$timestamp, breaks = "week", start.on.monday = FALSE)) + 6
      y <- ddply(y, "week", summarise, min(salesrank, na.rm = TRUE))
      names(y)[2] <- c(paste(names(salesRanks)[i], "Sales Rank", sep = " "))
    } 
    
    ## Get rid of Infs
    y[y == Inf] <- NA
    
    return(y)
  })
  
  names(salesRanks) <- tmpNames
  
  salesRanks
}