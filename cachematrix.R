## Special structure, that caches matrix and its inversion
## to do not repeat potentially time-consuming inverse calculation
## 
## This code made for peer-review assigment of R Programming course by Roger D. Peng
## 

## makeCacheMatrix creates a 'structure', using "<<-", 
## that able to store matrix and its inversed matrix

makeCacheMatrix <- function(x = matrix()) {
   # Initialize 'inversed', which will store inverse matrix
   inversed <- NULL
   # Storing function
   set <- function(y) {
             x        <<- y
             inversed <<- NULL
   }
   # Getting matrix
   get <- function() x
   # Setting inversed matrix
   setinversed <- function(z) inversed <<- z
   # Getting inversed matrix
   getinversed <- function() inversed
   # Returning all functions (making 'structure')
   list(set         = set, 
        get         = get,
        setinversed = setinversed,
        getinversed = getinversed)
}


## cacheSolve finds inversed matrix, if it still not stored,
## and returns cached value, if it already exists 

cacheSolve <- function(x, ...) {
   # Trying to obtain if inversed matrix already stored
   z <- x$getinversed()
   if(!is.null(z)) {
      message("getting cached data")
      return(z)
   }
   # If inversed matrix was not stored then it will be calculated
   data <- x$get()
   z <- solve(data, ...)
   # ... and stored in cache structure
   x$setinversed(z)
   # ... and returned
   z
}
