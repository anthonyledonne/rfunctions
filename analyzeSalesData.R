## Looks at book sales data from PRH Author Portal

analyzeSalesData <- function(salesData) {
  library(PerformanceAnalytics)
  
  sales <- read.csv(salesData, stringsAsFactors = FALSE )
  
  ## Dump last row
  sales <- sales[1:(nrow(sales)-1), ]
  
  ## Change factors to numeric
  sales[, 2:length(sales)] <- as.numeric(as.character(unlist(sales[, 2:length(sales)])))
  
  ## Change Week.Ending to Date
  sales$Week.Ending <- as.Date(sales$Week.Ending, "%m/%d/%y")
  
  ## Replace 0s with NA
  sales[sales == 0] <- NA
  
  ## Reorder columns based on publication date, L to R
  target <- names(sort(apply(sales[2:8], 2, function(x) which.min(is.na(x)))))
  sales <- sales[, c(1, match(target, colnames(sales)))]
  
  chart.Correlation(sales[, 2:length(sales)], method = "spearman", main = "Correlation Coefficients, [Spearman]")
  
  ## returns the rows of release weeks
  ## sales[sort(apply(sales[2:8], 2, function(x) which.min(is.na(x)))), ]
}