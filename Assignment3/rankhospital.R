rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")    
    
    ## Check that state and outcome are valid
    if (!(state %in% data[,'State'])) {
        stop('invalid state')
    }
    
    
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop('invalid outcome')
    }
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if (outcome == 'heart attack') {
        rank <- split(data[,11],data[,'State'])
    } else if (outcome == 'heart failure'){
        rank <- split(data[,17],data[,'State'])
    } else if (outcome == 'pneumonia'){
        rank <- split(data[,23],data[,'State'])
    }
    
    name <- split(data[,2],data[,'State'])
    
    state_rank <- as.numeric(rank[[state]])
    state_name <- name[[state]]
    
    new_data <- data.frame(rate = state_rank, hospital = state_name)
    new_data <- new_data[order(new_data$rate, new_data$hospital, na.last = NA),]
    
    # print(new_data)
    
    if (num == 'best') {
        return(new_data[1,'hospital'])
    } else if (num == 'worst') {
        return(new_data[nrow(new_data), 'hospital'])
    } else if (num > length(state_name)) {
        return(NA)
    } else {
        return(new_data[num, 'hospital'])
    }
}