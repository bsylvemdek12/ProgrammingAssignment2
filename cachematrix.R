## The first function is basically an object that allows for storing of the matrix
## if it has been solved already. It gets or sets similar to what you do in object
## oriented programming. x stores the matrix to be solved, m stores the inverse.

## This function handles storing the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  get <-function() x
  
  setMtrx <- function(solve) m<<- solve
  
  getMtrx <- function() m
  
  list (set=set, get=get, setMtrx=setMtrx, getMtrx=getMtrx)
}


## This function solves the matrix by first checking to see if it is in cache.
## If it is it returns the inverse from cache otherwise it calculates the inverse then
## stores it in cache then returns the value.
## A message is printed if retrieved from cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getMtrx()
  
  if(!is.null(m)){
    
    message("getting cached data")
    #Return from cache
    return(m)
    
  }
  data <- x$get()
  
  m <- solve(data,...)
  
  x$setMtrx(m)
  
  #Return not from cache
  m
}
