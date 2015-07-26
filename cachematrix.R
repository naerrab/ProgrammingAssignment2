## makeCacheMatrix sets up the functions and caches the inverse of a matrix
## set - It is a constructor and initializes the variables
## get - gets the passed in matrix 
## setinverse - sets the inverse of a matrix as a cached content
## getinverse - returns the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(y) {
          x <<- y
          inverse <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) inverse <<- inv
      getinverse <- function () inverse
      l <- list (get = get, set=set, setinverse=setinverse, getinverse = getinverse )
      l
}


## cacheSolve attempts to compute the inverse or returns the cached inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(is.null(inverse))
        {
          # Calculate Inverse
          print("Calculating Inverse and Setting")
          localmat <- x$get()
          inverse <- solve(localmat)
          x$setinverse(inverse)
          inverse
        }
        else
        {
           print("Getting Cached Mean")
           return(inverse) 
        }
}
