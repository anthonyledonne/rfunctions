compileUnitsSoldIntoDataFrame <- function(directory) {
  library(plyr)
  source("ingestCSVsFromDirectory.R")
  
  tmp <- ingestCSVsFromDirectory(directory, 10)
  
  ## Do some cleaning and polishing...
  tmp <- lapply(seq(tmp), function(i) {
    y <- data.frame(tmp[[i]])
    
    ## Take only what we want
    y <- y[1:(nrow(y)-1), 1:2]
    
    ## Change the column names
    names(y) <- c("Week.Ending", names(tmp)[i])
    
    ## Force correct classes for columns
    y[, names(tmp)[i]] <- as.numeric(gsub(",", "", y[, 2]))
    y$Week.Ending <- as.Date(y$Week.Ending, "%m/%d/%y")
    
    return(y)
  })
  
  ## Bring everything together
  tmp <- join_all(tmp, by = "Week.Ending", type = "full")
  tmp <- tmp[order(tmp$Week.Ending), ]
  tmp
}