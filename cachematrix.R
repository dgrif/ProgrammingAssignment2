## Essentially with these two functions I've mimicked functionality from the 
## example program for Vectors, substituting mean with solve to calculate 
## the inverse in the cacheSolve function. 

## The makeCacheMatrix function creates the special matrix that's just a 
## list with a function to:
# 1) set the matrix value
# 2) get the matrix value
# 3) set the inverse of the matrix
# 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, 
       setinv=setinv, getinv=getinv)
}


## The cacheSolve function calculates the inverse of a matrix, but checks to 
## see if the inverse was calculated previously done, if so it returns that value

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # check if the inv exists
  if(!is.null(inv)) {
    message("getting cached data.")
    # if so, return it
    return(inv)
  }
  # getter for 
  data <- x$get()
  # calculate the inverse
  inv <- solve(data)
  # and set it
  x$setinv(inv)
  inv
}
