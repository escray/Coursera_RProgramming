corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        
        fnames <- list.files(directory, full.names = TRUE)
        index <- 1
        result <- array()
        for (i in fnames) {
                data <- read.csv(i)
                complete <- data[complete.cases(data), ]
                
                if (nrow(complete) >= threshold){
                        result[index] <- round(cor(complete$nitrate, complete$sulfate), digit = 5)
                        index <- index + 1
                }
                
        }
        if (is.na(result[1])) {
                result <- 0
        }
        result
}