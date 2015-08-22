## Function to make a special matrix object that can cache its inverse when
## computer second function to compute inverses but calling for a cache first

## This function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        # setting function
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        # getting function
        x
    }
    setinverse <- function(value) {
        # set inverse within scope
        inverse <<- value
    }
    getinverse <- function() {
        inverse
    }
        
    list(
        set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## assumption matrix is always invertable
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
