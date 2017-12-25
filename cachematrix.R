## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL     #Inverse Variable will hold the Inverse matrix                            
  set <- function(y) { #Setting the value of the vector                              
    x <<- y             # upating old matrix to new matrix                      
    inverse <<- NULL                          
  }
  get <- function() x      #function will get the actual matrix                         
  setinverse <- function(solve) inverse <<- solve   
  getinverse <- function() inverse                  
  list(set = set, get = get,                        
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()                     # holds the inverse of the matrix
  if(!is.null(inverse)) {                       # check if this inverse of the matrix has been calculated
    message("getting cached data")        
    return(inverse)                      
  }
  data <- x$get()                               # get the actual matrix       
  inverse <- solve(data, ...)                   # calculating the inverse of the matrix using solve
  x$setinverse(inverse)                         # updating the variable that holds the inverse of the matrix
  inverse              
}
