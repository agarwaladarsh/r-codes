best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        data <- read.csv(paste(getwd(), "/data/prog_assgnmnt_3/outcome-of-care-measures.csv", sep = ""), colClasses = "character")
        dataframe  <- as.data.frame(cbind(data[, 2],   # hospital
                                    data[, 7],   # state
                                    data[, 11],  # heart attack
                                    data[, 17],  # heart failure
                                    data[, 23]), # pneumonia
                              stringsAsFactors = FALSE)
        colnames(dataframe) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if(!state %in% dataframe[, "state"]){
                stop('invalid state')
        } else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('invalid outcome')
        } else {
                state_indices <- which(dataframe[, "state"] == state) ## state indices
                state_data <- dataframe[state_indices, ]    # extracting data for the called state
                outcome_values <- as.numeric(state_data[, eval(outcome)]) ## outcome values for the state
                min_val <- min(outcome_values, na.rm = TRUE) ## min value
                result  <- state_data[, "hospital"][which(outcome_values == min_val)] ## result set
                output  <- result[order(result)] ## ordered result set
        }
        return(output)
}