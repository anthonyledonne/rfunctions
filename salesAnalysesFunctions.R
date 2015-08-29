sales <- sapply(seq(1:2), function(i) {
  sapply(seq(unitsSold), function(j) {
    y <- data.frame(unitsSold[[j]])
    y <- y[i,2]
  })
})
boxplot(sales)


###########################################################################################################################
##                                                                                                                       ##
##                                                     N Week Sales                                                      ##
##                                                                                                                       ##
###########################################################################################################################
firstNWeeksSales <- function(dataAsDataFrame, n) {
  sum(dataAsDataFrame[1:n, 2], na.rm = TRUE)
}
###########################################################################################################################
lastNWeeksSales <- function(dataAsDataFrame, n) {
  sum(dataAsDataFrame[nrow(dataAsDataFrame):(nrow(dataAsDataFrame)-(n-1)) , 2])
}
###########################################################################################################################




library(ggplot2)
library(reshape2)

sales.melted <- melt(sales)

## A basic box with the conditions colored
ggplot(sales.melted, aes(x=L1, y=value, group = L1)) + geom_boxplot()