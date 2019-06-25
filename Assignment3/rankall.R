rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")    
    
    ## Check that state and outcome are valid
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop('invalid outcome')
    }
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    if (outcome == 'heart attack') {
        rank <- split(data[,11],data[,'State'])
    } else if (outcome == 'heart failure'){
        rank <- split(data[,17],data[,'State'])
    } else if (outcome == 'pneumonia'){
        rank <- split(data[,23],data[,'State'])
    }
    
    name <- split(data[,2],data[,'State'])
    state_name <- unique(data[,'State'])
    
    # print(state_name)
    
    chos <- character()
    cstate <- character()
    
    for (st in state_name) {
        state_rank <- as.numeric(rank[[st]])
        state_name <- name[[st]]
        new_data <- data.frame(rate = state_rank, hos = state_name)
        new_data <- new_data[order(new_data$rate, new_data$hos, na.last = NA),]
        
        if (num == 'best') {
            chos <- c(chos, as.character(new_data[1,'hos']))
            cstate <- c(cstate, st)

        } else if (num == 'worst') {
            chos <- c(chos, as.character(new_data[nrow(new_data), 'hos']))
            cstate <- c(cstate, st)
            
        } else if (num > length(state_name)) {
            chos <- c(chos, 'NA')
            cstate <- c(cstate, st)
        } else {
            # append(chos, as.character(new_data[num, 'hos']))
            chos <- c(chos, as.character(new_data[num, 'hos']))
            cstate <- c(cstate, st)
        }
    }
    
    # print(chos)
    # print(cstate)
    
    final_result <- data.frame(hospital = chos, state = cstate)
    final_result <- final_result[order(final_result$state),]
    final_result
}