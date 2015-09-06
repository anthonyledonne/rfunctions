
## Note, if method is 'daily' or 'hourly', you won't be able to link Sales Ranks and Units Sold, 
## since Units Sold comes to us in a weekly format. That also means everything else downstream
## will be broken too. Needs to be "weekly" for that stuff to work.

method = "hourly" ## weekly, daily, or hourly

releaseDates = list("After the Kiss" = as.POSIXct("2013-08-26 00:00:00"),
                    "Isn't She Lovely" = as.POSIXct("2013-10-28 00:00:00"), 
                    "Love the One You're With" = as.POSIXct("2013-12-09 00:00:00"),
                    "Just One Night" = as.POSIXct("2014-04-22 00:00:00"),
                    "Only with You" = as.POSIXct("2014-07-29 00:00:00"),
                    "Broken" = as.POSIXct("2014-09-02 00:00:00"),
                    "Made for You" = as.POSIXct("2014-10-28 00:00:00"),
                    "The Trouble with Love" = as.POSIXct("2015-03-03 00:00:00"),
                    "Crushed" = as.POSIXct("2015-04-14 00:00:00"),
                    "Frisk Me" = as.POSIXct("2015-07-28 00:00:00"),
                    "Blurred Lines" = as.POSIXct("2015-08-25 00:00:00"))

releaseDates.df <- ldply(releaseDates, data.frame)
names(releaseDates.df) <- c("Title", "Date")


###########################################################################################################################
##                                                                                                                       ##
##                                                      Sales Ranks                                                      ##
##                                                                                                                       ##
###########################################################################################################################
source("~/Dropbox/Data Science Stuff/rfunctions/assembleSalesRankData.R")
salesRanks <- assembleSalesRankData("~/Dropbox/Data Science Stuff/apps/data/salesRank", method = method)
rm(assembleSalesRankData, ingestCSVsFromDirectory)

## Currently broken for method == weekly
source("~/Dropbox/Data Science Stuff/rfunctions/chartsForListOfSalesRanks.R")
bookCharts <- chartsForListOfSalesRanks(salesRanks = salesRanks, events = releaseDates)
rm(chartsForListOfSalesRanks)

##################################

## Testing some ideas here...
## Select the lowest rank of each day/week
salesRanks.lowest <- lapply(salesRanks, function(x) {
  
  ## Get the dataframe in question
  y <- data.frame(x)
  
  ## Cut by day
  y$day <- as.Date(cut(y$timestamp, breaks = "day"))
  
  ## Get the median salesrank of each day
  names(y)[2] <- "salesrank"
  y <- ddply(y, "day", summarise, "dailyMedian" = median(salesrank, na.rm = TRUE))
  
  ## Cut by week
  y$week <- as.Date(cut(y$day, breaks = "week", start.on.monday = FALSE)) + 6
  
  ## Get the median salesrank of each week
  y <- ddply(y, "week", summarise, "Weekly Median" = median(dailyMedian, na.rm = TRUE))
  
  ## Change the column names
  names(y) <- c("timestamp", paste(names(salesRanks[[i]][2]), "Weekly Median Sales Rank", sep = " "))
  
  return(y)
})

###########################################################################################################################
##                                                                                                                       ##
##                                                      Units Sold                                                       ##
##                                                                                                                       ##
###########################################################################################################################
source("~/Dropbox/Data Science Stuff/rfunctions/assembleUnitsSoldData.R")
unitsSold <- assembleUnitsSoldData("~/Dropbox/Data Science Stuff/apps/data/unitsSold")
rm(assembleUnitsSoldData, ingestCSVsFromDirectory)

unitsSold <- lapply(seq(unitsSold), function(i) {
  y <- data.frame(unitsSold[[i]])
  rownames(y) <- y$timestamp
  y <- y[-1]
  return(y)
})

###########################################################################################################################
##                                                                                                                       ##
##                                                   List of Data Pairs                                                  ##
##                                                                                                                       ##
###########################################################################################################################

if (method == "weekly" & length(unitsSold) == length(salesRanks)) {
  joined <- vector(mode = "list", length = length(unitsSold))
  joined <- lapply(seq(joined), function(i) {
    y <- join_all(list(salesRanks[[i]], unitsSold[[i]]),
                  by = "timestamp", type = "full")
    #y <- y[complete.cases(y), ]
    y <- y[order(y$timestamp), ]
    rownames(y) <- NULL
    return(y)
  })
  
  names(joined) <- names(unitsSold)
}


###########################################################################################################################
##                                                                                                                       ##
##                                                 Promotional Schedule                                                  ##
##                                                                                                                       ##
###########################################################################################################################

promos <- ingestCSVsFromDirectory("data/promoSchedule")
tmpNames <- names(promos)

## Do some cleaning and polishing...
promos <- lapply(seq(promos), function(i) {
  y <- data.frame(promos[[i]])
  
  ## Change the column names
  names(y) <- c("timestamp", names(promos)[i])
  
  ## Fix timestamp format
  y$timestamp <- as.Date(y$timestamp, "%m/%d/%y")
  
  ## Select the lowest rank of each day/week
  y$week <- as.Date(cut(y$timestamp, breaks = "week", start.on.monday = FALSE)) + 6
  y <- data.frame(tapply(y[, 2], y$week, min, na.rm = TRUE))
  
  ## Fix rownames and bring back timestamp
  y$timestamp <- as.Date(rownames(y))
  rownames(y) <- NULL
  
  ## Change the column names
  names(y) <- c(paste(names(promos)[i], "Promo", sep = " "), "timestamp")
  
  ## Reorder columns
  y <- y[c(2, 1)]
  
  return(y)
})
names(promos) <- tmpNames
#promos.df <- join_all(promos, by = "timestamp", type = "full")


#####
## Join joined (sales rank & units sold) with promo schedule
#####
if (exists("unitsSold") && exists("promos") && (length(unitsSold) == length(promos))) {
  unitsSold.withPromo <- vector(mode = "list", length = length(promos))
  unitsSold.withPromo <- lapply(seq(unitsSold.withPromo), function(i) {
    y <- join_all(list(unitsSold[[i]], promos[[i]]),
                  by = "timestamp", type = "full")
    
    y <- y[order(y$timestamp), ]
    rownames(y) <- NULL
    return(y)
  })
  
  names(unitsSold.withPromo) <- names(unitsSold)
  
}
# 
# joined.withPromos <- vector(mode = "list", length = length(unitsSold))
# joined.withPromos <- lapply(seq(joined.withPromos), function(i) {
#   y <- join_all(list(unitsSold[[i]], promoSchedule),
#                 by = "timestamp", type = "full")
#   
#   y <- y[complete.cases(y), ]
#   
#   return(y)
# })




###########################################################################################################################
##                                                                                                                       ##
##                                        CCFs Between Sales Ranks and Units Sold                                        ##
##                                                                                                                       ##
###########################################################################################################################

if (exists("joined")) {
  ccfs <- vector(mode = "list", length = length(joined))
  ccfs <- lapply(seq(joined), function(i) {
    ccfs[[i]] <- ccf(joined[[i]][2], joined[[i]][3], lag.max = 20,
                     main = paste(names(joined)[i], ": Sales Ranks & Units Sold ", sep = " "))
  })
  names(ccfs) <- names(unitsSold)
}


###########################################################################################################################
##                                                                                                                       ##
##                                                         Fits                                                          ##
##                                                                                                                       ##
###########################################################################################################################

if (exists("joined")) {
  fits <- vector(mode = "list", length = length(joined))
  fits <- lapply(seq(joined), function(i) {
    z <- data.frame(joined[[i]])
    sold <- z[,3]
    rank <- z[,2]
    fits[[i]] <- lm(sold ~ rank, data = z)
  })
  names(fits) <- names(unitsSold)
  
  lapply(fits, summary)
}



###########################################################################################################################
##                                                                                                                       ##
##                                                   CCFs between books                                                  ##
##                                                                                                                       ##
###########################################################################################################################

if(daily) {
  crossCorrelationBetweenBooks <- function(Book1, Book2) {
    library(plyr)
    
    ## Takes two numeric vectors as inputs
    ## and computes their cross-correlation
    x <- Book1
    y <- Book2
    
    ## Merge the two dataframes
    dataset <- merge(x, y, by = "timestamp", all = TRUE)
    
    ## Ensure ranks are numeric
    dataset[,2] <- as.numeric(dataset[,2])
    dataset[,3] <- as.numeric(dataset[,3])
    
    ## Complete Cases
    dataset <- dataset[complete.cases(dataset), ]
    
    ## Force timestamp to Date
    dataset$timestamp <- as.Date(dataset$timestamp)
    
    ## Create time series
    x.ts <- ts(dataset[,2])
    y.ts <- ts(dataset[,3])
    
    ## Find Min and Max CCF
    d <- ccf(x.ts, y.ts, lag.max = 20, plot = TRUE, ylab = "CCF", main = paste(names(Book1[2]), "&", names(Book2[2]), sep = " "))
    cor = d$acf[,,1]
    lag = d$lag[,,1]
    res = data.frame(cor,lag)
    res_min = res[which.min(res$cor),]
    res_max = res[which.max(res$cor),]
    
    rownames(res_min) <- NULL
    rownames(res_max) <- NULL
    
    list("max" = res_max, "min" = res_min)
  }
}


## https://onlinecourses.science.psu.edu/stat510/node/33
library(astsa)

BOOK_TO_TEST <- salesRanks$`Blurred Lines`

par(mfrow = c(2,2))
lapply(salesRanks, function(x) {
  if(!identical(names(BOOK_TO_TEST)[2], names(x)[2])) {
    crossCorrelationBetweenBooks(BOOK_TO_TEST, x)
    #acf(ts(x[complete.cases(x), 2]), lag.max = 50, main = names(x)[2])
  }
})

book <- ts(salesRanks$Broken[2])
lag1 <- lag(book, 1)
lag2 <- lag(book, 2)

alldata <- ts.intersect(book, lag1, lag2, lag3, lag4)
fit <- lm(book ~ lag1+lag2+lag3+lag4, data = alldata)
summary(fit)


lapply(salesRanks, function(x) crossCorrelationBetweenBooks(salesRanks$`Blurred Lines`, x))

lapply(salesRanks, function(x) NEWcrossCorrelationBetweenBooks(salesRanks$`Blurred Lines`, x, 39))

###########################################################################################################################
##                                                                                                                       ##
##                                                       Instagram                                                       ##
##                                                                                                                       ##
###########################################################################################################################
source("retrieveInstagramLikesAndCommentsForUsername.R")

## Import Instagram data
instagram <- retrieveInstgramLikesAndCommentsForUsername("_laurenlayne")

## Cut by week
instagram$week <- as.Date(cut(instagram$timestamp, breaks = "week")) - 2

## Split out likes and comments
if (daily) {
  likes <- subset(instagram, select = c(1, 3)) ## daily
  comments <- subset(instagram, select = c(1, 2)) ## daily  
} else if (!daily) {
  likes <- subset(instagram, select = c(4, 3))
  comments <- subset(instagram, select = c(4, 2))
}


instagram <- list("likes" = likes, "comments" = comments)
instagram <- lapply(seq(instagram), function(i) {
  
  ## Get the data frame in question
  y <- data.frame(instagram[[i]])
  
  ## Sum the variable (likes or comments) by week
  if (daily) {
    y <- data.frame(tapply(y[,2], as.Date(y$timestamp), sum, na.rm = TRUE)) ## daily, in case we ever want it
  } else if (!daily) {
    y <- data.frame(tapply(y[,2], y$week, sum, na.rm = TRUE))
  }
  
  ## Fix rownames and bring back timestamp
  y$timestamp <- as.Date(rownames(y))
  rownames(y) <- NULL
  
  ## Change the column names
  names(y) <- c(names(instagram)[i], "timestamp")
  
  ## Reorder columns
  y <- y[c(2, 1)]
  
  return(y)
})


###########################################################################################################################
##                                                                                                                       ##
##                                                 Stich stuff together                                                  ##
##                                                                                                                       ##
###########################################################################################################################

if (!daily) {
  
  joinTheseTwo <- function(A, B) {
    output <- lapply(seq(A), function(i) {
      
      ## Get the data frame in question
      y <- data.frame(A[[i]])
      
      ## Join_all with comments
      y <- join_all(list(y, B), by = "timestamp", type = "full")
      
      ## Complete Cases
      y <- y[complete.cases(y), ]
    })
    output
  }
  
  salesRanksAndLikes <- joinTheseTwo(salesRanks, instagram[[1]]) 
  salesRanksAndComments <- joinTheseTwo(salesRanks, instagram[[2]])
  unitsSoldAndLikes <- joinTheseTwo(unitsSold, instagram[[1]])
  unitsSoldAndComments <- joinTheseTwo(unitsSold, instagram[[2]])
  instagram.lists <- list(salesRanksAndLikes, salesRanksAndComments, unitsSoldAndLikes, unitsSoldAndComments)
}


############################################
##                  CCFs                  ##
############################################
if(exists("instagram.lists")) {
  ccfs.instagram <- vector(mode = "list", length = length(joined))
  ccfs.instagram <- lappy(seq(instagram.lists), function(i) {
    output <- lapply(seq(i), function(j) {
      
    })
  })
}

############################################
## CCFs for Instagram: Sales rank & Likes ##
############################################

ccfs.instagram <- vector(mode = "list", length = length(joined))
ccfs.instagram <- lapply(seq(salesRanksAndLikes), function(i) {
  ccfs.instagram[[i]] <- ccf(salesRanksAndLikes[[i]][3], salesRanksAndLikes[[i]][2], lag.max = 20, 
                             main = paste(names(unitsSold)[i], ": Sales rank (y) & Likes (x)", sep = " "))
})
names(ccfs.instagram) <- names(unitsSold)

###############################################
## CCFs for Instagram: Sales rank & Comments ##
###############################################

ccfs.instagram <- vector(mode = "list", length = length(joined))
ccfs.instagram <- lapply(seq(salesRanksAndComments), function(i) {
  ccfs.instagram[[i]] <- ccf(salesRanksAndComments[[i]][3], salesRanksAndComments[[i]][2], lag.max = 20, 
                             main = paste(names(unitsSold)[i], ": Sales rank (y) & Comments (x)", sep = " "))
})
names(ccfs.instagram) <- names(unitsSold)

############################################
## CCFs for Instagram: Units sold & Likes ##
############################################

ccfs.instagram <- vector(mode = "list", length = length(joined))
ccfs.instagram <- lapply(seq(unitsSoldAndLikes), function(i) {
  ccfs.instagram[[i]] <- ccf(unitsSoldAndLikes[[i]][3], unitsSoldAndLikes[[i]][2], lag.max = 20, 
                             main = paste(names(unitsSold)[i], ": Units sold (y) & Likes (x)", sep = " "))
})
names(ccfs.instagram) <- names(unitsSold)

###############################################
## CCFs for Instagram: Units sold & Comments ##
###############################################

ccfs.instagram <- vector(mode = "list", length = length(joined))
ccfs.instagram <- lapply(seq(unitsSoldAndComments), function(i) {
  ccfs.instagram[[i]] <- ccf(unitsSoldAndComments[[i]][3], unitsSoldAndComments[[i]][2], lag.max = 20, 
                             main = paste(names(unitsSold)[i], ": Units sold (y) & Comments (x)", sep = " "))
})
names(ccfs.instagram) <- names(unitsSold)


#########################################
########### Fits for Instagram ##########
#########################################

fits.instagram <- vector(mode = "list", length = length(joined))
fits.instagram <- lapply(seq(unitsSoldAndLikes), function(i) {
  z <- data.frame(unitsSoldAndLikes[[i]])
  fits.instagram[[i]] <- lm(z[,3] ~ z[,2], data = z)
})
names(fits.instagram) <- names(unitsSold)

lapply(fits.instagram, summary)




###########################################################################################################################
##                                                                                                                       ##
##                                                       Facebook                                                        ##
##                                                                                                                       ##
###########################################################################################################################

library(httr)
library(Rfacebook)

#token <- fbOAuth(app_id="935912299784361", app_secret="9c9fe4df2d7077ea92bfcc8f270c0fa0", extended_permissions = TRUE)

## Need to get this manually every time. https://developers.facebook.com/tools/explorer/
token <- "CAACEdEose0cBAICy242KMqSxYtnZCzC8aYiWXnwIL4ytkilZAzeFT4v2a2jIF8mna35HH7VDZB5NPJsJTsqGfsec7ZB5QIdnFL4Bloh6HoeZBePZAeWk2HhaHHX7tKB2brByfASptZB7TDg1zScyyUlV4eUFbeLVzD9C0P15znp0ti9gOU5s7XaFGoufpKjh1Nv2ldQ6CjiXQZDZD"

## Import facebook page data
page <- getPage("LaurenLayneAuthor", token = token, n = 1500)

## Get rid of extraneous information
page <- page[-c(1,2,3,6,7)]

## Fix timestamp
page$created_time <- as.POSIXct(page$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
page$created_time <- format(page$created_time, tz = "America/Chicago" )
page$created_time <- as.POSIXct(page$created_time, format = "%Y-%m-%d %H:%M:%S")

## Cut by week
page$week <- as.Date(cut(page$created_time, breaks = "week")) - 2

## Split out likes and comments
likes <- subset(page, select = c(6, 3))
comments <- subset(page, select = c(6, 4))

## Some cleaning and polishing...
tmp <- list("likes" = likes, "comments" = comments)
tmp <- lapply(seq(tmp), function(i) {
  
  ## Get the data frame in question
  y <- data.frame(tmp[[i]])
  
  ## Sum the variable (likes or comments) by week
  y <- data.frame(tapply(y[,2], y$week, sum, na.rm = TRUE))
  
  ## Fix rownames and bring back timestamp
  y$timestamp <- as.Date(rownames(y))
  rownames(y) <- NULL
  
  ## Change the column names
  names(y) <- c(names(tmp)[i], "timestamp")
  
  ## Reorder columns
  y <- y[c(2, 1)]
  
  return(y)
})


salesRanksAndFacebookLikes <- joinTheseTwo(salesRanks, tmp[[1]])
salesRanksAndFacebookComments <- joinTheseTwo(salesRanks, tmp[[2]])
unitsSoldAndFacebookLikes <- joinTheseTwo(unitsSold, tmp[[1]])
unitsSoldAndFacebookLikes <- joinTheseTwo(unitsSold, tmp[[2]])

############################################
## CCFs for Facebook : Sales rank & Likes ##
############################################

ccfs.facebook <- vector(mode = "list", length = length(joined))
ccfs.facebook <- lapply(seq(salesRanksAndFacebookLikes), function(i) {
  ccfs.facebook[[i]] <- ccf(salesRanksAndFacebookLikes[[i]][3], salesRanksAndFacebookLikes[[i]][2], lag.max = 20, 
                            main = paste(names(unitsSold)[i], ": Sales rank (y) & FB Likes (x)", sep = " "))
})
names(ccfs.facebook) <- names(unitsSold)

###############################################
## CCFs for Facebook : Sales rank & Comments ##
###############################################

ccfs.facebook <- vector(mode = "list", length = length(joined))
ccfs.facebook <- lapply(seq(salesRanksAndFacebookComments), function(i) {
  ccfs.facebook[[i]] <- ccf(salesRanksAndFacebookComments[[i]][3], salesRanksAndFacebookComments[[i]][2], lag.max = 20, 
                            main = paste(names(unitsSold)[i], ": Sales rank (y) & FB Comments (x)", sep = " "))
})
names(ccfs.facebook) <- names(unitsSold)

############################################
## CCFs for Facebook : Units Sold & Likes ##
############################################

ccfs.facebook <- vector(mode = "list", length = length(joined))
ccfs.facebook <- lapply(seq(salesRanksAndFacebookLikes), function(i) {
  ccfs.facebook[[i]] <- ccf(salesRanksAndFacebookLikes[[i]][3], salesRanksAndFacebookLikes[[i]][2], lag.max = 20, 
                            main = paste(names(unitsSold)[i], ": Units Sold (y) & FB Likes (x)", sep = " "))
})
names(ccfs.facebook) <- names(unitsSold)

###############################################
## CCFs for Facebook : Units Sold & Comments ##
###############################################

ccfs.facebook <- vector(mode = "list", length = length(joined))
ccfs.facebook <- lapply(seq(salesRanksAndFacebookComments), function(i) {
  ccfs.facebook[[i]] <- ccf(salesRanksAndFacebookComments[[i]][3], salesRanksAndFacebookComments[[i]][2], lag.max = 20, 
                            main = paste(names(unitsSold)[i], ": Units Sold (y) & FB Comments (x)", sep = " "))
})
names(ccfs.facebook) <- names(unitsSold)


###########################################################################################################################
##                                                                                                                       ##
##                                Stich together the Instagram & Facebook Likes & Comments                               ##
##                                                                                                                       ##
###########################################################################################################################
