best <- function(state, outcome) 
  { 
      valid_outcomes <- c("heart attack","heart failure","pneumonia") 
      if(outcome %in% valid_outcomes) 
      { 
          file <- read.csv(file="outcome-of-care-measures.csv",colClasses = "character") 
          filterData <- file[c(2, 7, 11, 17, 23)] 
          names(filterData) <- c("name","state","heart attack","heart failure","pneumonia") 
          if (state %in% filterData[,"state"])  
          { 
              validData <- filterData[,"state"] == state   &   filterData[,outcome] != "Not Available"               
              minPos <- which.min(filterData[,outcome][validData])   # give the position of the lower outcome rate 
              name <- filterData[,"name"][validData][minPos]         # get hospital name 
              name
          } else stop("invalid state")        
      } 
      else stop("invalid outcome") 
}

best("TX", "heart attack")

best("TX", "heart failure")

best("MD", "heart attack")

best("MD", "pneumonia")

best("BB", "heart attack")

best("NY", "hert attack")


best("SC", "heart attack")

best("NY", "pneumonia")

best("AK", "pneumonia")