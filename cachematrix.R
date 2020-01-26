#The assignment requires make two functions. First function is 'makeCacheMatrics.'
#According to the description of Coursera, 'makeCacheMatrix' is a fuction which can cache its inverse. 
#So, I made a function used to cache the result, then made the other function for computing the inverse through the solve function. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setinverse,
         getInverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinverse(inv)
    inv
}