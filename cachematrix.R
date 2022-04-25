## Functions to cache inverse matrix calculations when it has already been done one time
## and the values haven't changed since last calculation

## Special matrix to support caching of its inverse
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## Calculates the inverse of special matrices defined in the above function, caching the result
## and using the cached value if no changes were made in the object since the last calculation
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
      message("Getting inverse matrix cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
