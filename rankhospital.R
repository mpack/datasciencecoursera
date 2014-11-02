rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data & clean input
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    state <- toupper(state)
    outcome <- tolower(outcome)
    
    ## Set state and outcome references
    validStates <- unique(outcomeData[,"State"])
    validOutcomes <- c("heart attack"=11,"heart failure"=17,"pneumonia"=23)
    
    ## Check that state is valid
    if(!(state %in% validStates)) {
        ## invalid state, throw error
        stop("invalid state")
    }
    
    ## Check that outcome is valid
    if(!(outcome %in% names(validOutcomes))) {
        ## invalid outcome, throw error
        stop("invalid outcome")
    }
    
    ## Set column names
    outcomeColumn <- validOutcomes[outcome]
    stateColumn <- "State"
    
    ## Convert data to numeric format
    outcomeData[,outcomeColumn] <- as.numeric(outcomeData[,outcomeColumn])
    
    ## Capture all rows where state is a match
    matchingStates <- outcomeData[,stateColumn] == state
    outcomeData <- subset(outcomeData, matchingStates)
    
    ## Sort by rank, then name and remove NA's
    i <- order(outcomeData[,outcomeColumn], outcomeData[,"Hospital.Name"])
    outcomeData <- outcomeData[i,]
    outcomeData <- na.omit(outcomeData)
    
    ## Sort list of hospitals by name and return name with given rank
    if(is.numeric(num)) {
        return(outcomeData[num,"Hospital.Name"])
    } else if(num == "best") {
        return(head(outcomeData[,"Hospital.Name"], n = 1))
    } else if(num == "worst") {
        return(tail(outcomeData[,"Hospital.Name"], n = 1))
    } else {
        return(NA)
    }
}