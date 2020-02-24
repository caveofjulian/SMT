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