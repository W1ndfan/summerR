corr <- function(directory, threshold = 0){
    
    num_of_com <- complete(directory)
    
    total_v <- c()
    
    for (i in 1 : 332){
        if (num_of_com[i,'nobs'] > threshold){
            
            file_name <- filename(i)
            
            data <- read.csv(file_name)
            
            x <- data[((!is.na(data[,'sulfate'])) & (!is.na(data[,'nitrate']))),'sulfate']
            y <- data[((!is.na(data[,'nitrate'])) & (!is.na(data[,'sulfate']))),'nitrate']
            
            total_v <- c(total_v, cor(x, y))
        }
    }
    
    total_v
}