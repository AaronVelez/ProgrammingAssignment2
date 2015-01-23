# Calculates the inverse of a matrix and stores it in the cache
# Aaron I. Velez Ramirez
#
#####
# The first function, makeCacheMatrix, creates a list of functions that get the original and 
# inverted matrices ('get' and 'getinvert', respectiveley), and caches the original and inverted matrix
# ('set' and 'setinvert'). It takes an invertible matrix as input.

makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL                                # Creates an empty object to cache the inverted matrix when calculated
      set <- function(y) {                      # Replaces old matrix with new one AND deletes old inverted matrix
            x <<- y
            mx <<- NULL
      }
      get <- function() x                       # Gets the original matrrix
      setinvert <- function(inv.matrix)         # Stores the inverted matrix it in the cache 
            mx <<- inv.matrix              
      getinvert <- function() mx                # Gets the inverted matrix stored in the cache
      list(set=set, get=get, setinvert=setinvert,        # Returns all functions as a list
           getinverto=getinvert)
}


#####
# The second function, cacheSolve, calculates the inverse of a matrix stored in 'x'. If the inverse
# has been already calculated and the matrix is identical to the "target" matrix 'y', it retrieves
# the cached inverted matrix stored in x. When needed, it calls functions stored in object 'x'.

cacheSolve <- function(x, y, ...) {
      change <- NULL
      if (!identical(x$get(), y)) {             # Test if the matrix has changed
            x$set(y)                            # If TRUE Sets the new matrix in x
            change <- TRUE
            message("Matrix has changed, replacing cache data")
      } else {
            change <- FALSE
      }
      mx <- x$getinvert()                       # Gets the inverted cached matrix
      if (!is.null(mx) && !change) {            # Test if the matrix has been already inverted and cached
            message("Getting cached data")
            return(mx)
      }
      message("Caching data")
      data <- x$get()                           # Calls the original matrix
      mx <- solve(data)                         # Return a matrix that is the inverse of 'x'
      x$setinvert(mx)                           # Calls the 'setinvert' function to cache the new inverted matrix 
      mx                                        # Prints the inverted matrix
}


#####
# Example
#
# Invertible matrices
invertible <- matrix(c(3,1,2,1,8,2,4,1,2,4,5,7,34,3,5,8), nrow = 4, ncol = 4)
invertible2 <- matrix(c(24, 5, -4, -12, 3, 2, -2, -5, 4), nrow = 3, ncol = 3)

x = makeCacheMatrix(invertible)
cacheSolve(x, invertible)           # Scenario 1, cache empty
cacheSolve(x, invertible)           # Scenario 2, inverted matrix in cache AND matrix has not changed
cacheSolve(x, invertible2)          # Scenario 3, inverted matrix in cache BUT matrix has changed
cacheSolve(x, invertible2)          # Again scenario 2

