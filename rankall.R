rankall <- function(outcome, num = "best")
{ 
  valid_outcomes <- c("heart attack","heart failure","pneumonia") 
  if(outcome %in% valid_outcomes) 
    { 
        file<- read.csv(file="outcome-of-care-measures.csv",colClasses = "character") 
        filterData<- file[c(2, 7, 11, 17, 23)] 
        names(filterData) <- c("name","state","heart attack","heart failure","pneumonia") 
        dFrameFilter<- filterData[filterData[outcome] != "Not Available", ]  
        index<- order(dFrameFilter$name)
        dFrameFilter <- dFrameFilter[index,] 
        index<- order(as.numeric(dFrameFilter[,outcome]))
        orderData<- dFrameFilter[index,]                 
        allStates<- unique(dFrameFilter[,"state"])
        allStates<- allStates[order(allStates)]
        output<- data.frame() 
        for(state in allStates)
        { 
            getStateData <- orderData[orderData$state == state, c(1,2)]
            if(is.numeric(num) & nrow(getStateData) <= num)  
            { 
                name <- NA 
            } 
            else 
            {        
                if(is.numeric(num)) name<- getStateData[,"name"][num] 
                else if(num == "best") name<- getStateData[,"name"][1] 
                else name   <- tail(getStateData, n=1)[,"name"]
            } 
            newRow <- data.frame(name,state)
            names(newRow) <- names(output) 
            output <- rbind(output,newRow) 
        }
        names(output) <- c("hospital","state") 
        output 
    } 
    else stop("invalid outcome")    
}

head(rankall("heart attack", 20), 10)

tail(rankall("pneumonia", "worst"), 3)

tail(rankall("heart failure"), 10)

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)