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
  
  ## Build List of Dygraphs
  bookCharts <- lapply(salesRanks, function(x) {
    ## Status
    print(names(x))
    ## Chart
    y <- dygraph(
      interpNA(as.xts(x), method = "linear"),
      main=paste(names(x), "Sales Rank", sep = " ")) %>%
      dyRangeSelector()
    return(y)
  })
  
  if(!is.null(events)) {
    addEventNew <- function(dygraph, eventInfo = NULL) {
      dyEvent(
        dygraph = dygraph,
        date = eventInfo,
        labelLoc = "bottom",
        color = "red", 
        strokePattern = "dashed")
    }
    
    # Add Event Lines
    bookCharts <- lapply(bookCharts, function(x) {
      Reduce(function(g, z) {
        g %>% dyEvent(dygraph = ., date = z, labelLoc = "bottom", color = "red")
      }, events, init = x)
    })
  }
}