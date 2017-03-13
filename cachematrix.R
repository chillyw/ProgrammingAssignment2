## The makeCacheMatrix function takes a matrix and creates a list that stores the 
## matrix and the inverse of the matrix.  cacheSolve takes a list from makeCacheMatrix
## and attempts to solve the Matrix, first by checking the cache, and if no cache exists,
## then running the sovle function and storing the output.

## makeCacheMatrix takes a matrix as input and returns a list that has four functions to 
## store & retrieve both a matrix and the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a list from makeCacheMatrix as input; it first checks to see if the matrix
## stored in the list has been solved & cached.  If it has, it returns the cached solve result.
## Otherwise, it solves the matrix, caches the result, and then returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
