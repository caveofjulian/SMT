library(psych)

# setup -------------------------------------------------------------------
df <- getDataFrame()
# Assigning all of them to a variable so we don't have to access it every time, also these variable names are 
# much more descriptive and is a way to decouple the code from the dataframe.
paid_hours <- df$HrPWork
volunteer_hours <- df$HrVWork
class_hours <- df$HrClass
study_hours <- df$HrStud

# Likert scale variables are not included because they consist of multiple variables
quantitive_variables <- c("HrPWork", "HrVWork", "HrClass", "HrStud")
range <- c(0, 60) # All variables share the same range. 

# Only 0.03% of the data should have a z-score of 3 or higher. We use this z-score to look for the outliers, 
# however, the results will be analyzed in the report to determine whether or not these are legit outliers and should
# be removed.
outliers_zscore <- 3


# Export summaries of descriptive analysis --------------------------------
# In this analysis there are no likert scale variables because they are measured with multiple variables.
# Please visit cronbach_analysis to gain its analysis
export_quantitive_analysis <- function(data_frame, file_name="quantitive_analysis") {
  analysis <- describe(data_frame[quantitive_variables], skew = F)
  export_all_data(analysis, file_name = file_name)
}

# Note: in the report we compare the means from the analysis above (WITH outliers) to this analyis (WITHOUT outliers)
export_quantitive_analysis_without_outliers <- function(data, file_name="quantative_analysis_without_outliers") {
  # We must ensure that data is of type list because we're unlisting its indexes. This is important because we don't 
  # want de tightly couple the this method to the variable naming and having to access it by that (having as a result
  # that we wouldn't be able to reuse it when we change the Z scores!). C will automatically bundle all vectors provided
  # into one vector, meaning that accessing index 1 won't return the first VECTOR, but the first actual number.
  if(!is.list(data)) {
    stop("data must be of type list!")
  }
  
  # Because we are not dealing with a dataframe (dataframes MUST have the same amount of rows), we have to make an
  # analysis for each separately, at least as far I know at this point. Maybe look back at this when we've got time?
  paid_hours_analysis <- describe(unlist(data[0]), skew = F)
  volunteer_hours_analyis <- describe(unlist(data[1]), skew = F)
  class_hours_analyis <- describe(unlist(data[2]), skew = F)
  study_hours_analyis <- describe(unlist(data[3]), skew = F)
  
  # Making this a list so it's being accepted as a sequence, basically a necessity due to the comment above
  export_all_data(list(paid_hours_analysis, volunteer_hours_analyis, class_hours_analyis, study_hours_analyis), 
                  file_name)
}

# Quantitive analysis (including outliers analysis) -----------------------
# Exports an analysis of the quantitive variables in the dataframe.
export_quantitive_analysis(df) 

hist(paid_hours, col = "Blue", ylim = range, xlab="Paid time (in hours/week)", las=1)
hist(volunteer_hours, col = "Blue", ylim = range, xlab="Volunteer time (in hours/week)", las=1)
hist(class_hours, col = "Blue", ylim = range, xlab="Class time (in hours/week)", las=1)
hist(study_hours, col = "Blue", ylim = range, xlab="Study time (in hours/week)", las=1)

boxplot(paid_hours,col="Blue",las=1)
boxplot(volunteer_hours,col="Blue",las=1)
boxplot(class_hours,col="Blue",las=1)
boxplot(study_hours,col="Blue",las=1)

paid_hours_zscores <- scale(paid_hours)
volunteer_hours_zscores <- scale(volunteer_hours)
class_hours_zscores <- scale(class_hours)
study_hours_zscores <- scale(study_hours)


paid_hours_outliers <- paid_hours[which(abs(paid_hours_zscores) > outliers_zscore)]
volunteer_hours_outliers <- volunteer_hours[which(abs(volunteer_hours_zscores) > outliers_zscore)]
class_hours_outliers <- class_hours[which(abs(class_hours_zscores) > outliers_zscore)]
study_hours_outliers <- study_hours[which(abs(study_hours_zscores) > outliers_zscore)]

paid_hours_without_outliers <- paid_hours[-which(abs(paid_hours_zscores) > outliers_zscore)]
volunteer_hours_without_outliers <- volunteer_hours[-which(abs(volunteer_hours_zscores) > outliers_zscore)]
class_hours_without_outliers <- class_hours[-which(abs(class_hours_zscores) > outliers_zscore)]
study_hours_without_outliers <- study_hours[-which(abs(study_hours_zscores) > outliers_zscore)]

mean(study_hours[-study_hours_extremes], na.rm = TRUE)

data_without_outliers <- list(paid_hours_without_outliers, volunteer_hours_without_outliers, class_hours_without_outliers,
                           study_hours_without_outliers)

# Exports all the relevant information to a file called quantitive analysis without outliers
export_quantitive_analysis_without_outliers(data_without_outliers)

# A Z-Score of 3 turned out to be very low for paid hours and volunteer hours, we want to look for > 45 for paid
# hours and for volunteer hours to > 20.
paid_hours_without_outliers_revisited <- paid_hours[-which(abs(paid_hours) > 40)]
volunteer_hours_without_outliers_revisited <- volunteer_hours[-which(abs(volunteer_hours) > 20)]

data_without_outliers_revisited <- 
  list(paid_hours_without_outliers_revisited, volunteer_hours_without_outliers_revisited,
       class_hours_without_outliers, study_hours_without_outliers) 

export_quantitive_analysis_without_outliers(data_without_outliers_revisited, 
                                            file_name = "quantative_analysis_without_outliers_revisited")



# Should become a package -------------------------------------------------
getDataFrame <- function(columns) {
  return(read.csv("SMT1920casus01.csv", stringsAsFactors = FALSE, sep = ";")[,columns])
}

export_all_data <- function(data, file_name) {
  if(missing(file_name)) {
    # This generates a UUID which is a way to ensure that there's no file with the same name on the disk which would 
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