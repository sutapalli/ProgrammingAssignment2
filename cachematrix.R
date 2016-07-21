## Creating temporary matrix
# Function to create a special type of matrix, a temp matrix
TempMatrix <- function(x = matrix()) {
  inverse <- NULL                                   
  set <- function(y) {            
    x <<- y                       
    inverse <<- NULL              
  }
  get <- function() x                               
  setinverse <- function(solve) inverse <<- solve   
  getinverse <- function() inverse                  
  list(set = set, get = get,                        
       setinverse = setinverse,
       getinverse = getinverse)
}

##This function checks if already inverse calculated or not
# if not, then it calculates inverse
CalInverse <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {                       
    return(inverse)                       
  }
  
  data <- x$get()                                      
  inverse <- solve(data, ...)                   
  x$setinverse(inverse)                         
  inverse 
}