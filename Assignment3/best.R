best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    
    if (!(state %in% data[,'State'])) {
        stop('invalid state')
    }
    
    
    if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {
        stop('invalid outcome')
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
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
    
    # print(state_rank)
    
    best_rate <- min(state_rank, na.rm = TRUE)
    
    # print(best_rate)
    
    rank_index <- numeric()
    
    for (i in (1:length(state_rank))) {
        if (state_rank[i] == best_rate & (!is.na(state_rank[i]))) {
            rank_index <- c(rank_index, i)
        }
    }
    
    cand <- state_name[rank_index]
    
    # print(cand)
    
    min(cand)
}