# Create the makeCacheMatrix function that creates a special matrix that can cache its inverse.
# Function gets values for matrix, sets the inverse, and gets the inverse of the matrix.
# The MASS package allows us to work with both square and non-squared matrices.

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
 
   
  inv <- NULL # initialize the inverse matrix with NULL values
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function()x  #This function gets the matrix x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function() {
    inver <- ginv(x)
    inver %*% x # function computes the inverse of the matrix 
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# The function cachemean calculates the mean of the special vector created above. However, it first checks
# if the mean has already been calculated. IF so, it gets the mean from the cache and skips the computation.
# Otherwise, it calculates the mean of the data and sets teh value of the men in the cache via the setmean 
# function

## get data from cache
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  #Check if inverese is NULL
  if(!is.null(inv)){
    message("getting cached data!") # If not NULL, get inverse from cache and inform user of such
    
    # If inverse not in cache, Function computes and returns inverse of the matrix
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
  