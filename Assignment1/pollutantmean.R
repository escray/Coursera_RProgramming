pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        newfile <- TRUE
        for (i in id) {
                while (nchar(i) < 3) {
                        i = paste("0", i, sep = "")
                }
                filepath = paste(directory, "/", i, ".csv", sep = "")
                ## read first file
                if (newfile) {
                        merge.data <- read.csv(filepath)
                        newfile <- FALSE
                }
                ## if multiple file then read and merge
                else {
                        new.data <- read.csv(filepath)
                        merge.data <- rbind(merge.data, new.data)
                }
        }
        
        if (pollutant == "nitrate") {
                result <- mean(merge.data$nitrate, na.rm = TRUE)
        }
        else {
                result <- mean(merge.data$sulfate, na.rm = TRUE)
        }
        
        round(result, digit = 3)
}