crossCorrelationBetweenBooks <- function(Book1, Book2) {
  
  x <- read.csv(Book1, stringsAsFactors = FALSE)
  y <- read.csv(Book2, stringsAsFactors = FALSE)
  
  ## Remove the last row, which is just text
  ## And only take the first two columns (timestamp and amazon.com)
  x <- x[1:(nrow(x)-2), 1:2]
  y <- y[1:(nrow(y)-2), 1:2]
  
  ## Change column names for programming ease
  colnames(x) <- c("timestamp", "rankX")
  colnames(y) <- c("timestamp", "rankY")
  
  ## Merge the two dataframes
  data <- merge(x, y, by = "timestamp", all = TRUE)
  
  ## Ensure ranks are numeric
  data$rankX <- as.numeric(data$rankX)
  data$rankY <- as.numeric(data$rankY)
  
  ## Simplyfy the timestamp from hours to days
  data$timestamp <- as.Date(data$timestamp)
  
  ## Complete Cases
  data <- data[complete.cases(data), ]
  
  ## Select the lowest rank of each day
  data <- cbind(tapply(data$rankX, data$timestamp, min),
                tapply(data$rankY, data$timestamp, min)
  )
  
  ## Back to dataframe
  data <- data.frame(data)
  
  ## Bring back the timestamp
  data$timestamp <- rownames(data)
  
  ## Get rid of rownames
  rownames(data) <- NULL
  
  ## Change Colnames
  colnames(data) <- c("rankX", "rankY", "timestamp")
  
  ## Change order of columns
  data <- data[, c(3, 1, 2)]
  
  ## Force timestamp to Date
  data$timestamp <- as.Date(data$timestamp)
  
  ## Create time series
  x.ts <- ts(data$rankX)
  y.ts <- ts(data$rankY)
  
  ## Find Min CCF
  d <- ccf(x.ts, y.ts, plot = TRUE, ylab = "cross-correlation", main = paste(Book1, "vs", Book2, sep = " "))
  cor = d$acf[,,1]
  lag = d$lag[,,1]
  res = data.frame(cor,lag)
  res_min = res[which.min(res$cor),]
  res_max = res[which.max(res$cor),]
  
  rownames(res_min) <- NULL
  rownames(res_max) <- NULL
  
  list("max" = res_max, "min" = res_min)
}