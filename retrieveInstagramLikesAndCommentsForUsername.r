retrieveInstgramLikesAndCommentsForUsername <- function(username, seconds_to_run = 7, n = 200) {
  #Analyze Instagram with R
  #Author: Julian Hillebrand
  
  #packages
  require(httr)
  require(rjson)
  require(RCurl)
  require(reshape2)
  
  
  #Authentication
  
  ## getting callback URL
  full_url <- oauth_callback()
  full_url <- gsub("(.*localhost:[0-9]{1,5}/).*", x=full_url, replacement="\\1")
  #print(full_url)
  #message <- paste("Copy and paste into Site URL on Instagram App Settings:", 
  #                full_url, "\nWhen done, press any key to continue...")
  
  #invisible(readline(message))
  
  app_name <- "LeDonneTest"
  client_id <- "0b5e5472174549209bb7d1758d5d4645"
  client_secret <- "06b6109acb7d4b02b58a768f7fcc1589"
  scope = "basic"
  
  
  instagram <- oauth_endpoint(
    authorize = "https://api.instagram.com/oauth/authorize",
    access = "https://api.instagram.com/oauth/access_token")  
  myapp <- oauth_app(app_name, client_id, client_secret)
  
  ig_oauth <- oauth2.0_token(instagram, myapp,scope="basic",  type = "application/x-www-form-urlencoded",cache=FALSE)  
  
  
  ## Token
  
  tmp <- strsplit(toString(names(ig_oauth$credentials)), '"')
  token <- tmp[[1]][4]
  
  ########################################################
  
  #username <- "_laurenlayne"
  
  #search for the username
  user_info <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/search?q=',username,'&access_token=',token,sep="")),unexpected.escape = "keep")
  
  received_profile <- user_info$data[[1]]
  
  if(grepl(received_profile$username,username))
  {
    user_id <- received_profile$id
    
    ## set dataframe
    med <- data.frame()
    
    ## let's have this run for X number of seconds
    #seconds_to_run <- 10
    
    timeout <- Sys.time() + seconds_to_run
    first_time <- TRUE
    while (Sys.time() <= timeout) {
      if(first_time) {
        media <- fromJSON(getURL(paste('https://api.instagram.com/v1/users/',user_id,'/media/recent/?access_token=',token,sep="")))
        first_time <- FALSE
      } else {
        if(length(media[[1]]$next_url)) {
          media <- fromJSON(getURL(paste(media[[1]]$next_url,sep="")),unexpected.escape = "keep")
        }
      }
      if(length(media$data)) {
        medias <- data.frame(no = 1:length(media$data))
        
        for(i in 1:length(media$data)) {
          
          #print(paste("getting meta of media", i, "of", length(media$data), sep=" "))
          
          ## comments
          medias$comments[i] <- media$data[[i]]$comments$count
          
          ## likes
          medias$likes[i] <- media$data[[i]]$likes$count
          
          ## date
          medias$date[i] <- toString(as.POSIXct(as.numeric(media$data[[i]]$created_time), origin="1970-01-01"))
        }
      }
      #print("Storing profiles and medias")
      med <- as.data.frame(rbind(med, medias))
    }
    
    
    ## 2 outputs, regular and melted
    ## regular output
    regular <- med[c(4,2,3)]
    regular <- unique(regular)
    names(regular)[1] <- "timestamp"
    regular$timestamp <- as.POSIXct(regular$timestamp, origin="1970-01-01")
    
    ## melted
    #melted <- melt(regular, id = "timestamp")
    
    ## put it together
    #list("regular" = regular, "melted" = melted)
    
    regular
    
  }else
  {
    print("Error: User not found!")
  }
}