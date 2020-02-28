# Packages to install to run this code 
# install.packages("psych", dependencies = TRUE)
library(psych)
# Essentially we want this in another file since the guid creation is only used for file generation to export the data
library(uuid)

# Clear memory
remove(list=ls())
# Clear console
cat("\f")

# The keys represent the column names for the particular Likert scales.
nep_keys = c("Nep01", "Nep02", "Nep03", "Nep04", "Nep05")
bif_keys = c("Bif01", "Bif02", "Bif03", "Bif04", "Bif05", "Bif06", "Bif07")
guilt_keys = c("Guilt01", "Guilt02", "Guilt03", "Guilt04", "Guilt05")
big_keys = c("Big02","Big07") # Only 2 and 7 are relevant because only they measure agreeableness

# The dataframes contain all data gathered with the likert scales.
nep_df <- getDataFrame(nep_keys)
bif_df <- getDataFrame(bif_keys)
guilt_df <- getDataFrame(guilt_keys)
big_df <- getDataFrame(big_keys)

# The chronbach's alpha indicates the internal consistency
alpha_nep <- alpha(nep_df, keys = nep_keys[c(2:4)])
alpha_bif <- alpha(bif_df, keys = bif_keys[2:7])
alpha_guilt <- alpha(guilt_df, keys = guilt_keys[c(3:5)])
alpha_big <- alpha(big_df, check.keys =  TRUE)


# These scores are special because it contains the means per every row
result_nep <- alpha_nep$scores
result_bif <- alpha_bif$scores
result_guilt <- alpha_guilt$scores
result_big <- alpha_big$scores

# Securing all data we gathered
export_all_data(list(alpha_nep, alpha_bif, alpha_guilt, alpha_big), file_name = "alpha_analysis")
export_all_data(list(result_nep, result_bif, result_guilt, result_big), file_name = "averages_results")

# Just to check we can print out the means which should correspond with the data gathered in the alpha_analysis file
# print(mean(result_nep))
# print(mean(result_bif))
# print(mean(result_guilt))
# print(mean(result_big))


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