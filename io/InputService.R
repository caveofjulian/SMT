getFullDataFrame <- function() {
  return(read.csv("SMT1920casus01.csv", stringsAsFactors = FALSE, sep = ";"))
}

getDataFrame <- function(column) {
  return(read.csv("SMT1920casus01.csv", stringsAsFactors = FALSE, sep = ";")[,column])
}

getMean <- function(column) {
  data <- getDataFrame(column)
  return(mean(data, na.rm = TRUE))
}