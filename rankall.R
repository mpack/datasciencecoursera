rankall <- function(outcome, num = "best") {
    ## Read outcome data & clean input
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcome <- tolower(outcome)
    
    ## Set state and outcome references
    states <- unique(outcomeData[,"State"])
    states <- sort(states)
    validOutcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
    
    ## Check that outcome is valid
    if(!(outcome %in% names(validOutcomes))) {
        ## invalid outcome, throw error
        stop("invalid outcome")
    }
    
    ## Set vars for loop
    outcomeColumn <- validOutcomes[outcome]
    stateColumn <- "State"
    outcomeData[,outcomeColumn] <- as.numeric(outcomeData[,outcomeColumn])
    
    ## For each state, find the hospital of the given rank
    hospitals <- c()
    outcomeDataTemp <- outcomeData
    for(state in states) {        
        ## Capture all rows where state is a match
        matchingStates <- outcomeData[,stateColumn] == state
        outcomeData <- subset(outcomeData, matchingStates)
        
        ## Sort by rank, then name and remove NA's
        i <- order(outcomeData[,outcomeColumn], outcomeData[,"Hospital.Name"])
        outcomeData <- outcomeData[i,]
        outcomeData <- na.omit(outcomeData)
        
        ## Sort list of hospitals by name and return name with given rank
        if(is.numeric(num)) {
            hospitals <- c(hospitals, outcomeData[num,"Hospital.Name"])
        } else if(num == "best") {
            hospitals <- c(hospitals, head(outcomeData[,"Hospital.Name"], n = 1))
        } else if(num == "worst") {
            hospitals <- c(hospitals, tail(outcomeData[,"Hospital.Name"], n = 1))
        } else {
            hospitals <- c(hospitals, NA)
        }
        
        ## Restore data to original form after each loop iteration
        outcomeData <- outcomeDataTemp
    }
    
    ## Return a data frame with the hospital names and the state name
    output <- data.frame('hospital'=hospitals, 'state'=states, row.names = states)
    return(output)
}