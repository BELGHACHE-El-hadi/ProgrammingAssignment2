## The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to

      ## set            : set the value of the matrix
      ## get            : get the value of the matrix
      ## setinverse     : set the value of the matrix inverse
      ## getinverse     : get the value of the matrix inverse

## x : is the matrix
## i : is the matrix inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}



## The following function calculates a matrix that is the inverse of 'x' or NA if 'x' is not inversible.
## However, it first checks to see if the matrix inverse has already been calculated. 
## If so, it `get`s the matrix inverse from the cache and skips the computation. 
## Otherwise, it calculates the matrix inverse of the cached matrix and sets the value of the matrix inverse
## in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {              
      i <- x$getinverse()
      
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      
      data <- x$get()
      
      ## A square matrix that does not have a matrix inverse (called 'singular') iff its determinant is 0.
      ## First of all, The function computes the dertmiant of the cached matrix 'data'
      det<-determinant.matrix(data, logarithm=F)[[1]][[1]]
      
      
      if (det==0 || is.na(det)) {
            ## If the determinant is zero or is NA then 'data' is singular and doesn't have a matrix inverse.
            ## So, it retruns an NA 
            i <- NA      
      }
      else{
            ## Otherwise 'data' has a matrix inverse. 
            ## It computes the matrix inverse with the 'solve' function and returns the result             
            i <- solve(data, ...)
      }
      
      x$setinverse(i)
      
      i
}