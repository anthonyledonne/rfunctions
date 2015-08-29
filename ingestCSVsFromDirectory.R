ingestCSVsFromDirectory <- function(directory, skip = 0) {
  ## Takes a directory of CSV files
  ## spits out a list of all the read CSVs
  ## with names <- filenames
  
  ## Create a list of files
  files_full <- list.files(directory, full.names=TRUE, pattern = ".csv")
  files_short <- list.files(directory, full.names = FALSE, pattern = ".csv")
  
  ## Create an empty list that's the length of our expected output.
  ## Input object will be files and empty list is going to be tmp.
  files <- vector(mode = "list", length = length(files_full))
  
  ## Read the cvs files and drop them into our list
  files <- lapply(files_full, function(x) read.csv(x, stringsAsFactors = FALSE, skip = skip))
  names(files) <- gsub(".csv", "", files_short)
  names(files) <- gsub("rank-", "", names(files))
  names(files) <- gsub("-", " ", names(files))
  names(files) <- gsub("edition", "", names(files))
  
  files
}