readMeasuredData <- function(filename) {
    
    require(data.table)
    require(lubridate)
    
    if (!file.exists(filename)) {
        stop(paste("The file '", filename, "' cannot be found!"))        
    }
    
    message(paste("Reading", filename, '...'))
    data <- fread(filename) 
    transform(data, ate = ymd(data$date))    
}