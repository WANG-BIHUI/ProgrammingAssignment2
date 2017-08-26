## A pair of functions that cache and compute the inverse of a matrix

## makeCacheMatrix is a function that can create a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }

  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    i <<- inverse
  }
  getInverse <- function() {
    m
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve is a function that can compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if( !is.null(m) ) {
          message("Getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data) %*% data
        x$setInverse(m)
        m      
}
