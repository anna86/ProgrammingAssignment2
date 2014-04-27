## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) c <<- inv
  getInverse <- function() c
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  c <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data)
  x$setInverse(c)
  c
}
