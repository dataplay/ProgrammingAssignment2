## The functions created here will obtain the inverse of Matrix x
## If the inverse of Matrix x has already be obtained and saved in cache, no calculation will be done and the stored value in cache will be used.

## The following function is to obtain the value of Matrix x and create a list of functions set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {       ## x is the matrix we try to obtain the inverse of using this function
    m <- NULL
    set <- function(y) {
        x <<-y
        m<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)  m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## The following function is to decide if the inverse of Matrix x needs to be calculated, in that case it will be calculated by the "solve" function. If the inverse of Matrix x has already been calculated, the cached value will be printed instead.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) { message("getting cached data")   ## if m is not empty, then cached data for x will be obtained for the inverse.
        return(m)
    }
    data <- x$get()    ## if m is empty, then the inverse of X will be calculated
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
    
}