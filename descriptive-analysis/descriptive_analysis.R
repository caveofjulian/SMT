

df <- getDataFrame()

# Note that we should get rid of Nep, Guilt, Big and Bif columns! 
# If I find the time I could write a regex for this but R does not seem to support negations due to its POSIX standard 
analysis <- describe(df, skew = FALSE)

export_all_data(analysis, file_name = "generic_data") 









# This should become a package so we can reuse this.
getDataFrame <- function(columns) {
  return(read.csv("SMT1920casus01.csv", stringsAsFactors = FALSE, sep = ";")[,columns])
}

export_all_data <- function(data, file_name) {
  if(missing(file_name)) {
    # This generates a UUID which is a way to ensure that there's no file with the same name on the PC which could 
    # cause issues. It is based on the datetime, garantueeing its uniqueness. 
    file_name <- gsub("[-]", "", uuid::UUIDgenerate()) 
  }
  capture.output(data, file = paste(file_name, ".txt"))
}

print_all_data <- function(data, decimals = 3) {
  for (info in data) {
    print(info)
  }
}