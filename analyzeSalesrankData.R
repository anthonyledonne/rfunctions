analyzeSalesRankData <- function(directory = getwd(), beta = FALSE, gamma = FALSE, h = 14) {
  library(plyr)
  library(forecast)
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
    
    return(y)
  })
  
  ## Bring everything together
  tmp <- join_all(tmp, by = "timestamp", type = "full")
  tmp <- tmp[order(tmp$timestamp), ]
  rownames(tmp) <- NULL
  ts.plot(ts(tmp[2:length(tmp)]), main = "", gpars = list(col = c("black", "red", "green", "blue", "purple")))
  
  
    
#   ## Create a list of time series objects for each book
#   books.ts <- lapply(tmp, function(x) ts(x[complete.cases(x), 2]))
#   
#   ##########
#   ##Outputs#
#   ##########
# 
#   ## HoltWinters Filtering (for each book)
#   Holt.Winters.Filtering <- lapply(books.ts, function(x) HoltWinters(x, beta = beta, gamma = gamma, l.start = x[1]))
#   
#   ## HoltWinters forecasting
#   Holt.Winters.Forecast <- lapply(Holt.Winters.Filtering, function(x) forecast.HoltWinters(x, h = h))
#   
#   ## Forecast Plots
#   Forecast.Plot <- lapply(seq(Holt.Winters.Forecast), function(i) plot.forecast(i, main = names(tmp)[i]))
#   
#   ## Auto Arima
#   auto.arima.output <- apply(books.ts, 2, auto.arima)
#   
#   ## Box Test
#   box.test.output <- lapply(names(holtwinters.smoothened2), function(x) Box.test(holtwinters.smoothened2[[x]]$residuals, lag=20, type="Ljung-Box"))
#   
#   # ACF of Residuals
#   acf.residuals <- lapply(names(holtwinters.smoothened2), function(x) acf(holtwinters.smoothened2[[x]]$residuals, lag.max = 20))
#   
#   list("HoltWinters.Smoothened" = Holt.Winters.Filtering,
#        "HoltWinters.Forecasting" = Holt.Winters.Forecast,
#        "Forecast.Plot" = forecast.plot,
#        "Auto.Arima" = auto.arima.output,
#        "Time.Series" = books.ts,
#        "Box.Test" = box.test.output,
#        "ACF" = acf.residuals
#        )
#   
#   ##https://a-little-book-of-r-for-time-series.readthedocs.org/en/latest/src/timeseries.html#time-series-analysis
#   ##lapply(names(holtwinters.smoothened2), function(x) acf(holtwinters.smoothened2[[x]]$residuals, lag.max = 20))
#   ##lapply(names(holtwinters.smoothened2), function(x) Box.test(holtwinters.smoothened2[[x]]$residuals, lag=20, type="Ljung-Box"))
}