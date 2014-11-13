## This code will create a matrix object and be able to cache its
## inverse. 

## makeCacheMatrix will take a matrix and cache it
makeCacheMatrix <- function(x = matrix()) {
  
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <-  function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## CacheSolve will take a matrix and check the cache to see if the 
## inverse has already been calculated. If not, it will calculate the
## inverse and store it. If it has already been cached, the inverse will
## be retrieved from the cache.
cacheSolve <- function(x, ...) {
    ## Get the inverse from the cache
    s <- x$getinverse()
    ## If the value exists in the cache, return it.
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    ## If not, calculate and store the inverse
    data <- x$get()
    s <- solve(data)
    x$setinverse(s)
    s
}
