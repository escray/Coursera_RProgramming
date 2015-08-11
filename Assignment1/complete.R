complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        ## print("##   id nobs")
        index <- 1
        ids <- array()
        nobs <- array()
        for (i in id) {
                origin <- i
                
                
                ## for file name
                while (nchar(i) < 3){
                        i = paste("0", i, sep = "")
                }
                filepath = paste(directory, "/", i, ".csv", sep = "")
                
                data <- read.csv(filepath)
                
                result <- nrow(data[complete.cases(data), ])
                
                ids[index] <- origin
                nobs[index] <- result
                
                
                index <- index + 1
        }
        tmp <- data.frame(ids, nobs)
        names(tmp)[1] <- "id"
        tmp
}