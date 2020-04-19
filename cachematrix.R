makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y    # Set value
    m <<- NULL # Clear cache
  }
  # Define function to get the value of the matrix
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  # Define function to get the inverse
  getInverse <- function() m
  
  # Return a list with the above four functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x) {
  m <- x$getInverse() 
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  # The cache was empty. We need to calculate it, cache it, and then return it.
  data <- x$get()  # value of matrix
  m <- solve(data) # Calculate inverse
  x$setInverse(m)  # Cache the result
  return(m)                # Return inverse
}

