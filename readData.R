readMeasuredData <- function(filename) {
    
    require(data.table)
    require(lubridate)
    require(dplyr)
    
    if (!file.exists(filename)) {
        stop(paste("The file '", filename, "' cannot be found!"))        
    }
    
    message(paste("Reading", filename, '...'))
    fread(filename) %>% transform(date = ymd(data$date))    
}