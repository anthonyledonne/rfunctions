compileSalesRankIntoDataFrame <- function(directory, hourly = FALSE) {
  library(plyr)
  source("ingestCSVsFromDirectory.R")
  
  tmp <- ingestCSVsFromDirectory(directory)
  
  ## Do some cleaning and polishing...
  tmp <- lapply(seq(tmp), function(i) {
    y <- data.frame(tmp[[i]])
    
    ## Take only what we want
    y <- y[1:(nrow(y)-2), 1:2]
    
    ## Change the column names
    names(y) <- c("timestamp", names(tmp)[i])
    
    ## Force correct classes for columns
    y[, names(tmp)[i]] <- as.numeric(gsub(",", "", y[, 2]))
    
    if(!hourly) {
      ## Daily
      
      y$timestamp <- as.Date(y$timestamp)
      
      ## Select the lowest rank of each day
      y <- data.frame(tapply(y[, 2], y$timestamp, min, na.rm = TRUE))
      
      ## Fix rownames and bring back timestamp
      y$timestamp <- as.Date(rownames(y))
      rownames(y) <- NULL
      
      ## Change the column names
      names(y) <- c(names(tmp)[i], "timestamp")
      
      ## Reorder columns
      y <- y[c(2, 1)]
      
      ## Get rid of Infs
      y[y == Inf] <- NA
    } else if(hourly) {
      ## Hourly
      y$timestamp <- as.POSIXct(y$timestamp, format = "%Y-%m-%d %H:%M:%S")
    }
    
    return(y)
  })
  
  ## Bring everything together
  tmp <- join_all(tmp, by = "timestamp", type = "full")
  tmp <- tmp[order(tmp$timestamp), ]
  rownames(tmp) <- NULL
  
  tmp
}