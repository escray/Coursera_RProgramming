best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
        ## Check that state and outcome are valid
        states <- levels(data$State)
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if( length(grep(state, data$State)) <= 0 ) {
                stop("invalid state")
        }
        
        
        column <- 0
        if (outcome == "heart attack" ){
                column <- 11
        }
        else if (outcome == "heart failure") {
                column <- 17
        }
        else if (outcome == "pneumonia") {
                column <- 23
        }
        else {
                stop("invalid outcome")
        }
                        
        df <- data[, c(2, 7, column)]
        names(df) <- c("hospital", "state", "value")
        df$value <- suppressWarnings(as.numeric(df$value))
        df <- df[complete.cases(df), ]
        
        orderdata <- df[df$state == state, ]
        orderdata <- orderdata[order(orderdata[, 3], orderdata[, 2]),]
        ## return(head(orderdata, 6))
        head(orderdata, 1)[1, 1]
        ## Return hospital name in the state with lowest 30-day death rate
}