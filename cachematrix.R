## Represents a matrix as a list of functions that operate on the underlying matrix.
## The functions available are: get (retrieve the matrix), set (sets the matrix),
## setInverse (sets the inverse of the matrix), getInverse (retrieves the inverse of the matrix)
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


## Computes the inverse of a cachable matrix (see makeCacheMatrix() for more information).
## If the inverse is computed before, it is retrieved from the cache. Otherwise, the inverse
## is computed and stored in the cache for later use.
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
