## The two functions below create a special matrix object that
## caches its own inverse for future computations.

## makeCacheMatrix() takes a matrix x and creates a list of functions that set
## and get x and also set and get the inverse of x. These functions are accessed
## using the $ notation.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL            ## Initializes inv as an empty vector
     set <- function(y) {
          x <<- y              ## Assigns a matrix y to x in the parent environment while
          inv <<- NULL         ## at the same time overwriting the previous inverse with a NULL value
     }
     get <- function() x
     setinv <- function(inverse) inv <<- inverse       ## Takes a matrix and assigns it to inv in the parent environment
     getinv <- function() inv
     list( set = set, get = get, setinv = setinv, getinv = getinv)  ## The special object created by makeCacheMatrix
}


## cacheSolve() takes one of the special matrixes created by makeCacheMatrix
## and returns the cached inverse matrix, if it exists. If it does not exist, it computes the inverse,
## caches it, and returns it.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if (!is.null(inv)) {                 ## Tests whether the inverse of x already exists in the cache
          message("getting cached data")  ## Displays a message if the cached value is not empty
          return(inv)                     ## Returns the cached value and exits the function
     }
     data <- x$get()                      ## If the cached inverse value is NULL, gets the matrix x and finds
     inv <- solve(data, ...)              ## the inverse, then caches it for future use
     x$setinv(inv)
     inv
}