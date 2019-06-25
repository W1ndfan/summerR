pollutantmean <- function(directory, pollutant, id = 1 : 332){
    temp <- FALSE
    final <- c()
    
    for (row_num in id) {
        if (row_num < 10){
            file_name <- paste('0','0', row_num, '.csv', sep = '')
        } else if (row_num < 100) {
            file_name <- paste('0', row_num, '.csv', sep = '')
        } else {
            file_name <- paste(row_num, '.csv', sep = '')
        }
        
        data <- read.csv(file_name)
        
        # if (temp == FALSE){
        #     final <- data[,pollutant]
        #     temp <- TRUE
        # } else {
        #     final <- cbind(final, data[, pollutant])
        #     ## final[[as.character(row_num)]] <- data[, pollutant]
        # }
        
        p_data <- data[, pollutant]
        p_data_log <- is.na(p_data)
        
        rm_p_data <- p_data[!p_data_log]
        
        final <- c(final, rm_p_data)
        
    #     print (rm_p_data)
    #     print (class(rm_p_data))
    }
    
    mean(final, na.rm = TRUE)
    
}