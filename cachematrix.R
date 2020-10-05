## Put comments here that give an overall description of what your
## functions do

## The function 'makeCacheMatrix' creates a special matrix object 
## that caches or stores the inverted matrix as an in-memory object
## to be retrieved when needed

  makeCacheMatrix <- function(x = matrix()) {
          s <- NULL
          set <- function(y) {
            x <<- y
            s <<- NULL
          }
          get <- function() x
          setinverse <- function(solve) s <<- solve
          getinverse <- function() s
          list(set = set, get = get,
               setinverse = setinverse,
               getinverse = getinverse)
  }


## cacheSolve computes what is the inverse of the matrix 
## (input argument is the output of makeCacheMatrix above)
## the cacheSolve function checks that if the matrix has not changed and 
## the inverse has been computed previously and stored in memory, 
## it will retrieve the inverse from memory instead of recomputing it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
          message("getting cached data")
          return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

