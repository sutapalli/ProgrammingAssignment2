rankhospital <- function(state, outcome, num = "best")
  {       
      valid_outcomes <- c("heart attack","heart failure","pneumonia") 
      if(outcome %in% valid_outcomes) 
      { 
          file <- read.csv(file="outcome-of-care-measures.csv",colClasses = "character") 
          filterData<- file[c(2, 7, 11, 17, 23)] 
          names(filterData) <- c("name","state","heart attack","heart failure","pneumonia") 
          if (state %in% filterData[,"state"])  
          {
              dFrameFilter <- filterData[filterData$state==state & filterData[outcome] != "Not Available", ] 
              if(is.numeric(num) & nrow(dFrameFilter) <= num)  
              { 
                  NA 
              } 
              else 
              {
                  index <- order(dFrameFilter$name)
                  dFrameFilter <- dFrameFilter[index,] 
                  index <- order(as.numeric(dFrameFilter[,outcome]))
                  if(is.numeric(num))     dFrameFilter[index,][,"name"][num] 
                  else if(num == "best")  dFrameFilter[index,][,"name"][1] 
                        else tail(dFrameFilter[index,], n=1)[,"name"]
              } 
          } 
          else stop("invalid state")    
    } 
    else stop("invalid outcome")    
} 

rankhospital("TX", "heart failure", 4)

rankhospital("MD", "heart attack", "worst")

rankhospital("MN", "heart attack", 5000)


rankhospital("NC", "heart attack", "worst")

rankhospital("WA", "heart attack", 7)

rankhospital("TX", "pneumonia", 10)

rankhospital("NY", "heart attack", 7)
