# setup -------------------------------------------------------------------

df <- getDataFrame()
# Likert scale variables are not included because they consist of multiple variables
quantitive_variables <- c("HrPWork", "HrVWork", "HrClass", "HrStud")


# Quantitive generic analysis ---------------------------------------------

# In this analysis there are no likert scale variables because they are measured with multiple variables.
# Please visit cronbach_analysis to gain its analysis
export_quantitive_analysis <- function(data_frame) {
  analysis <- describe(data_frame[quantitive_variables], skew = F)
  export_all_data(analysis, file_name = "quantitive_analysis")
}

export_quantitive_analysis(df) 

# Outliers analysis --------------------------------------------------------
# Assigning all of them to a variable so we don't have to access it every time, also these variable names are 
# much more descriptive so we can understand the code.
paid_hours <- df$HrPWork
volunteer_hours <- df$HrVWork
class_hours <- df$HrClass
study_hours <- df$HrStud

# All variables share the same range. Although paid_hours ends at 59 we want steps of 10, so this is acceptable.
range <- c(0, 60)

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

# Only 0.03% of the data should have a z-score of 3 or higher. We use this z-score to look for the outliers, 
# however, the results will be analyzed in the report to determine whether or not these are legit outliers and should
# be removed.
outliers_zscore <- 3

paid_hours_outliers <- paid_hours[which(abs(paid_hours_zscores) > outliers_zscore)]
volunteer_hours_outliers <- volunteer_hours[which(abs(volunteer_hours_zscores) > outliers_zscore)]
class_hours_outliers <- class_hours[which(abs(class_hours_zscores) > outliers_zscore)]
study_hours_outliers <- study_hours[which(abs(study_hours_zscores) > outliers_zscore)]

paid_hours_without_outliers <- paid_hours[-which(abs(paid_hours_zscores) > outliers_zscore)]
volunteer_hours_without_outliers <- volunteer_hours[-which(abs(volunteer_hours_zscores) > outliers_zscore)]
class_hours_without_outliers <- class_hours[-which(abs(class_hours_zscores) > outliers_zscore)]
study_hours_without_outliers <- study_hours[-which(abs(study_hours_zscores) > outliers_zscore)]


# Note: the old means are part of the quantitive analysis that's been exported before.  
export_quantitive_analysis_different_row_count <- function(data_frame, data) {
  # Because we are not dealing with a dataframe (dataframes MUST have the same amount of rows), we have to make an
  # analysis for each separately, at least as far I know at this point. Maybe look back at this when we've got time?
  paid_hours_analysis <- describe(data[1], skew = F)
  volunteer_hours_analyis <- describe(data[2], skew = F)
  class_hours_analyis <- describe(data[3], skew = F)
  study_hours_analyis <- describe(data[4], skew = F)
  
  # Making this a list so it's being accepted as a sequence, basically a necessity due to the comment above
  export_all_data(list(paid_hours_analysis, volunteer_hours_analyis, class_hours_analyis, study_hours_analyis), 
                  file_name = "quantitive_analysis_without_outliers")
}
 mean(study_hours[-study_hours_extremes], na.rm = TRUE)

data_without_outliers <- list(paid_hours_without_outliers, volunteer_hours_without_outliers, class_hours_without_outliers,
                           study_hours_without_outliers)
export_quantitive_analysis_without_outliers(df[quantitive_variables], data_without_outliers)


paid_hours_mean_without_outliers <- mean(paid_hours[-paid_hours_extremes], na.rm = TRUE)
volunteer_hours_mean_without_outliers <- mean(volunteer_hours[-volunteer_hours_extremes], na.rm = TRUE)
class_hours_mean_without_outliers <- mean(class_hours[-class_hours_extremes], na.rm = TRUE)
study_hours_mean_without_outliers <-


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