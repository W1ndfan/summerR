filename <- function(id){
    
    if (id < 10){
        name <- paste('00',id,'.csv',sep = '')
    } else if (id < 100) {
        name <- paste('0',id,'.csv', sep = '')
    } else {
        name <- paste(id,'.csv',sep = '')
    }
    
    name
}


complete <- function(directory, id = 1 : 332){
    
    mon_id <- c()
    mon_nobs <- c()
    
    for (i in 1:(length(id))){
        monitor <- id[[i]]
        
        file_name <- filename(monitor)
        
        data <- read.csv(file_name)
        
        log_s <- !(is.na(data[,'sulfate']))
        log_n <- !(is.na(data[,'nitrate']))
        
        com <- log_s & log_n
        
        # print(com)
        
        mon_id <- c(mon_id,monitor)
        mon_nobs <- c(mon_nobs, sum(com))
    }
    
    final <- data.frame(id = mon_id, nobs = mon_nobs)
    final
}