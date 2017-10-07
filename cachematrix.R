#### MAKECACHEMATRIX ####

# This first function creates a list containing a function to:
#   Get/set for matrix inverse .
#   Return list of functions for the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(inverse) i <<- inverse
  geti <- function() i
  list(set = set, get = get, seti = seti, geti = geti)
}	

#### CACHESOLVE ####

# The following function calculates the inverse of the  matrix created with the above function.
# However, it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached result")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}

#### CHECK (optional) ####

# To check that the above functions work, just run the following:

z <- matrix(runif(25),5,5)
print(z)
z1<- makeCacheMatrix(z)
cacheSolve(z1)
