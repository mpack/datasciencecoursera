## The function best takes two arguments: the 2-character abbreviated name
## of a state and an outcome name. The function reads the
## outcome-of-care-measures.csv and returns a character vector
## with the name of the hospital that has the best (i.e. lowest)
## 30-day mortality for the specied outcome in that state. 

best <- function(state, outcome) {
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
    
    ## Capture all values for matching outcome
    mortalityRates <- outcomeData[,outcomeColumn]
    
    ## Filter by state and find min mortality
    mortalityRates <- subset(mortalityRates, matchingStates)
    
    ## Find min mortality rate for state and outcome
    lowestMortalityRate <- min(mortalityRates, na.rm = TRUE)
    
    ## Grab all hospital names in matching state return those matching lowest mortality rate
    hospitals <- outcomeData[,"Hospital.Name"]
    hospitals <- subset(hospitals, matchingStates)
    bestHospitals <- subset(hospitals, mortalityRates == lowestMortalityRate)
    
    ## Sort list of hospitals by name and return first
    bestHospitals <- sort(bestHospitals)
    bestHospitals[1]
}