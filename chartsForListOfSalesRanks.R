###########################################################################################################################
##                                                                                                                       ##
##                                                  Sales Ranks Graphs                                                   ##
##                                                                                                                       ##
###########################################################################################################################

## Takes a list of Amazon sales ranks and returns a list of Dygraphs
## Optionally takes a list of event dates and adds event lines to each of the Dygraphs
chartsForListOfSalesRanks <- function(salesRanks, events = NULL) {
  
  ## Load libraries
  library(xts)
  library(timeSeries)
  library(dygraphs)
  
  
  ## Prep work, moving timestamp column to rowname
  salesRanks <- lapply(salesRanks, function(x) {
    y <- data.frame(x)
    rownames(y) <- y$timestamp
    y <- y[-1]
    return(y)
  })
  
  
  ## Build List of Dygraphs from list of salesranks
  bookCharts <- lapply(salesRanks, function(x) {
    ## Status
    print(names(x))
    ## Chart
    y <- dygraph(
      data = interpNA(as.xts(x), method = "linear"),
      main = paste(names(x), "Sales Rank", sep = " "))
    
    if(!is.null(events)) {
      for(event in names(events)) {
        y <- y %>% dyEvent(dygraph = .,
                           date = events[[event]],
                           label = event,
                           labelLoc = "bottom",
                           color = "red")
      }
    }
    return(y)
  })
}